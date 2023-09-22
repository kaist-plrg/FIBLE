open PCode;;
open DS;;
open Domain;;

type cfa_astate = {
  analysis_contour: ContourD.t;
  sound_jump: JumpD.t;
  abs_state: FSAbsD.t
}

let join (c1: cfa_astate) (c2: cfa_astate): cfa_astate = {
  analysis_contour = ContourD.join c1.analysis_contour c2.analysis_contour;
  sound_jump = JumpD.join c1.sound_jump c2.sound_jump;
  abs_state = FSAbsD.join c1.abs_state c2.abs_state
}

let ole (c1: cfa_astate) (c2: cfa_astate): bool = (ContourD.ole c1.analysis_contour c2.analysis_contour) && (JumpD.ole c1.sound_jump c2.sound_jump) && (FSAbsD.ole c1.abs_state c2.abs_state)

type heuristic_result =
 | HrUnsound of LocSet.t
 | HrSound of LocSet.t
 | HrExit
 | HrFallthrough

 let reposition_sn (vs: (int * inst list)) (s: loc): loc =
 match vs with
 | (v, is) -> if (snd s) < (List.length is) then s else (Int64.add (fst s) (Int64.of_int v), 0)

let reposition_sn_set (vs: (int * inst list)) (s: LocSet.t): LocSet.t =
  LocSet.map (reposition_sn vs) s

let fallthru (p: prog) (l: loc): loc =
 let nl = (fst l, succ (snd l)) in
 match p.ins_mem (fst l) with
  | Some vs -> reposition_sn vs nl
  | None -> nl

type flow_heurstic_type = prog -> loc -> AbsState.t -> inst -> heuristic_result

let gen_bb_single (h: flow_heurstic_type) (p: prog) (l: loc) (a: AbsState.t) (i: inst): (loc option * loc option) =
  match h p l a i with
  | HrUnsound s -> (None, None)
  | HrSound s -> (None, None)
  | HrExit -> (None, Some l)
  | HrFallthrough -> (Some (fallthru p l), Some l)

let gen_ujump_single (h: flow_heurstic_type) (p: prog) (l: loc) (a: AbsState.t) (i: inst): LocSet.t option =
  match h p l a i with
  | HrUnsound s -> Some s
  | HrSound s -> None
  | HrExit -> None
  | HrFallthrough -> None

let gen_jump_single (h: flow_heurstic_type) (p: prog) (l: loc) (a: AbsState.t) (i: inst): LocSet.t =
  match h p l a i with
  | HrUnsound s -> LocSet.empty
  | HrSound s -> s
  | HrExit -> LocSet.empty
  | HrFallthrough -> LocSet.empty

let flow_heuristic_simple (p: prog) (l: loc) (a: AbsState.t) (i: inst): heuristic_result = match i with
| Ijump (Jcall, vn) -> HrFallthrough
| Ijump (_, vn) -> (match vn with
  |  {
    varNode_node = Ram a ;
    varNode_width = _ } -> HrSound (LocSet.singleton (a, 0))
  | _ -> HrUnsound (LocSet.empty)
)
| Ijump_ind (JIret, vn) -> HrExit
| Ijump_ind (JIcall, vn) -> HrFallthrough
| Ijump_ind (_, vn) -> HrUnsound (LocSet.empty)
| Icbranch (cn, jn) -> (match jn with
  |  {
    varNode_node = Ram a ;
    varNode_width = _ } -> HrSound (LocSet.of_list ((a, 0) :: (fallthru p l) :: []))
  | _ -> HrUnsound (LocSet.empty))
| Iunimplemented -> HrUnsound (LocSet.empty)
| _ -> HrSound (LocSet.singleton (fallthru p l))

let gen_contour (p: prog) (a: AbsState.t LocBotMap.t): (LocSetOption.t LocBotMap.t * (LocSet.t * LocSet.t) * LocSet.t LocBotMap.t) =
  let u = LocBotMap.mapi (fun l a -> match get_ins p l with
    | Some i -> gen_ujump_single flow_heuristic_simple p l a i
    | None -> None
  ) a in
  let b = LocBotMap.fold (fun l a (bens, bexs) ->
    let ins = get_ins p l in
    match ins with
    | Some i -> let (eno, exo) = gen_bb_single flow_heuristic_simple p l a i in
                (Option.fold ~none:bens ~some:(fun l -> LocSet.add l bens) eno, Option.fold ~none:bexs ~some:(fun l -> LocSet.add l bexs) exo)
    | None -> (bens, bexs)
  ) a (LocSet.empty, LocSet.empty) in
  let j = LocBotMap.mapi (fun l a -> match get_ins p l with
    | Some i -> gen_jump_single flow_heuristic_simple p l a i
    | None -> LocSet.empty
  ) a in
  (u, b, j)


let post_contour_fs (p: prog) (c: ContourD.t) (sj: LocSet.t LocBotMap.t) (a: AbsState.t LocBotMap.t): AbsState.t LocBotMap.t =
  let locHash = LocHashtbl.create 100 in
  LocBotMap.iter (fun ls s ->
    match (get_ins p ls, LocBotMap.find_opt ls a) with
    | (Some ins, Some ae) ->
      LocSet.iter (fun lf ->
      LocHashtbl.update locHash lf (fun v -> match v with
        | Some af -> Some (AbsState.join af (AbsState.post_contour_single p ls lf ae ins))
        | None -> Some (AbsState.post_contour_single p ls lf ae ins)
      )
    ) s
    | _ -> ()
  ) sj;
  LocSet.iter (fun l -> LocHashtbl.update locHash l (fun v -> Some AbsState.top)
  ) (fst c.basic_block);
  LocHashtbl.to_seq locHash |> LocBotMap.of_seq 

let post (p: prog) (c: cfa_astate): cfa_astate =
  let (u, b, j) = gen_contour p (c.abs_state) in
  let (nu, nb, nj) = (LocBotMap.mapjoin LocSetOption.join c.analysis_contour.unsound_jump u,
                      (fun (a, b) (c, d) -> (LocSet.join a c, LocSet.join b d)) c.analysis_contour.basic_block b,
                      LocBotMap.mapjoin LocSet.join c.sound_jump j) in
  let ncontour: ContourD.t = {unsound_jump = nu; basic_block = nb} in
  {analysis_contour = ncontour; sound_jump = nj; abs_state = post_contour_fs p ncontour nj c.abs_state}

let init (p: prog) (l: loc): cfa_astate =
  { analysis_contour = {unsound_jump = LocBotMap.empty; basic_block = (LocSet.singleton l, LocSet.empty)};
    sound_jump = LocBotMap.empty;
    abs_state = LocBotMap.singleton l AbsState.top
  }

let rec a_fixpoint (p: prog) (c: cfa_astate): cfa_astate =
  let nc = join c (post p c) in
  if ole nc c then c else a_fixpoint p nc

let follow_flow (p: prog) (e: addr): cfa_astate = a_fixpoint p (init p (e, 0))