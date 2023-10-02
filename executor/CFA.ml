open StdlibExt
open Basic
open Basic_domain
open Value_domain
open State_domain

type heuristic_result =
  | HrUnsound of LocSetD.t
  | HrSound of LocSetD.t
  | HrExit
  | HrFallthrough

let reposition_sn (vs : int * Inst.t list) (s : Loc.t) : Loc.t =
  match vs with
  | v, is ->
      if snd s < List.length is then s
      else (Int64.add (fst s) (Int64.of_int v), 0)

let reposition_sn_set (vs : int * Inst.t list) (s : LocSetD.t) : LocSetD.t =
  LocSetD.map (reposition_sn vs) s

let fallthru (p : Prog.t) (l : Loc.t) : Loc.t =
  let nl = (fst l, succ (snd l)) in
  match p.ins_mem (fst l) with Some vs -> reposition_sn vs nl | None -> nl

type flow_heurstic_type =
  Prog.t -> Loc.t -> AbsState.t -> Inst.t -> heuristic_result

let gen_bb_single (h : flow_heurstic_type) (p : Prog.t) (l : Loc.t)
    (a : AbsState.t) (i : Inst.t) : Loc.t option * Loc.t option =
  match h p l a i with
  | HrUnsound s -> (None, None)
  | HrSound s -> (None, None)
  | HrExit -> (None, Some l)
  | HrFallthrough -> (Some (fallthru p l), Some l)

let gen_ujump_single (h : flow_heurstic_type) (p : Prog.t) (l : Loc.t)
    (a : AbsState.t) (i : Inst.t) : LocSetD.t option =
  match h p l a i with
  | HrUnsound s -> Some s
  | HrSound s -> None
  | HrExit -> None
  | HrFallthrough -> None

let gen_jump_single (h : flow_heurstic_type) (p : Prog.t) (l : Loc.t)
    (a : AbsState.t) (i : Inst.t) : LocSetD.t =
  match h p l a i with
  | HrUnsound s -> LocSetD.empty
  | HrSound s -> s
  | HrExit -> LocSetD.empty
  | HrFallthrough -> LocSetD.empty

let flow_heuristic_simple (p : Prog.t) (l : Loc.t) (a : AbsState.t) (i : Inst.t)
    : heuristic_result =
  match i with
  | Ijump (Jcall, vn) -> HrFallthrough
  | Ijump (_, vn) -> (
      match vn with
      | { varNode_node = Ram a; varNode_width = _ } ->
          HrSound (LocSetD.singleton (a, 0))
      | _ -> HrUnsound LocSetD.empty)
  | Ijump_ind (JIret, vn) -> HrExit
  | Ijump_ind (JIcall, vn) -> HrFallthrough
  | Ijump_ind (_, vn) -> (
      match AbsState.try_concretize_vn a vn 20 with
      | Some a ->
          HrSound
            (LocSetD.of_seq (Int64Set.to_seq a |> Seq.map (fun a -> (a, 0))))
      | None -> HrUnsound LocSetD.empty)
  | Icbranch (cn, jn) -> (
      match jn with
      | { varNode_node = Ram a; varNode_width = _ } ->
          HrSound (LocSetD.of_list [ (a, 0); fallthru p l ])
      | _ -> HrUnsound LocSetD.empty)
  | Iunimplemented -> HrUnsound LocSetD.empty
  | _ -> HrSound (LocSetD.singleton (fallthru p l))

module Immutable = struct
  type __ = {
    analysis_contour : ContourD.t;
    sound_jump : JumpD.t;
    abs_state : FSAbsD.t;
  }

  include
    TripleD.MakeJoinSemiLattice_Record (ContourD) (JumpD) (FSAbsD)
      (struct
        type t = __

        let get_fst x = x.analysis_contour
        let get_snd x = x.sound_jump
        let get_trd x = x.abs_state
        let make x y z = { analysis_contour = x; sound_jump = y; abs_state = z }
      end)

  let gen_contour (p : Prog.t) (a : FSAbsD.t) :
      UJumpD.t * BoundaryPointD.t * JumpD.t =
    let u =
      FSAbsD.mapi
        (fun l a ->
          match Prog.get_ins p l with
          | Some i -> gen_ujump_single flow_heuristic_simple p l a i
          | None -> None)
        a
    in
    let b =
      FSAbsD.fold
        (fun l a (bens, bexs) ->
          let ins = Prog.get_ins p l in
          match ins with
          | Some i ->
              let eno, exo = gen_bb_single flow_heuristic_simple p l a i in
              ( Option.fold ~none:bens ~some:(fun l -> LocSetD.add l bens) eno,
                Option.fold ~none:bexs ~some:(fun l -> LocSetD.add l bexs) exo
              )
          | None -> (bens, bexs))
        a
        (LocSetD.empty, LocSetD.empty)
    in
    let j =
      FSAbsD.mapi
        (fun l a ->
          match Prog.get_ins p l with
          | Some i -> gen_jump_single flow_heuristic_simple p l a i
          | None -> LocSetD.empty)
        a
    in
    (u, b, j)

  let post_contour_fs (p : Prog.t) (c : ContourD.t) (sj : JumpD.t)
      (a : FSAbsD.t) : FSAbsD.t =
    let locHash = LocHashtbl.create 100 in
    FSAbsD.iter
      (fun ls s ->
        match (Prog.get_ins p ls, FSAbsD.find_opt ls a) with
        | Some ins, Some ae ->
            LocSetD.iter
              (fun lf ->
                LocHashtbl.update locHash lf (fun v ->
                    match v with
                    | Some af ->
                        Some
                          (AbsState.join af
                             (AbsState.post_contour_single p ls lf ae ins))
                    | None -> Some (AbsState.post_contour_single p ls lf ae ins)))
              s
        | _ -> ())
      sj;
    LocSetD.iter
      (fun l -> LocHashtbl.update locHash l (fun v -> Some AbsState.top))
      (fst c.boundary_point);
    LocHashtbl.to_seq locHash |> FSAbsD.of_seq

  let post (p : Prog.t) (c : t) : t =
    let u, b, j = gen_contour p c.abs_state in
    let nu, nb, nj =
      ( UJumpD.join c.analysis_contour.unsound_jump u,
        (fun (a, b) (c, d) -> (LocSetD.join a c, LocSetD.join b d))
          c.analysis_contour.boundary_point b,
        JumpD.join c.sound_jump j )
    in
    let ncontour : ContourD.t = { unsound_jump = nu; boundary_point = nb } in
    {
      analysis_contour = ncontour;
      sound_jump = nj;
      abs_state = post_contour_fs p ncontour nj c.abs_state;
    }

  let init (p : Prog.t) (l : Loc.t) : t =
    {
      analysis_contour =
        {
          unsound_jump = UJumpD.empty;
          boundary_point = (LocSetD.singleton l, LocSetD.empty);
        };
      sound_jump = JumpD.empty;
      abs_state = FSAbsD.singleton l AbsState.top;
    }

  let rec a_fixpoint (p : Prog.t) (c : t) : t =
    let nc = join c (post p c) in
    if le nc c then c else a_fixpoint p nc

  let follow_flow (p : Prog.t) (e : Addr.t) : t = a_fixpoint p (init p (e, 0))
end
