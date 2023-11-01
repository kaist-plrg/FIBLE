open StdlibExt
open Basic
open Basic_domain
open Basic_collection
open Value_domain

type heuristic_result =
  | HrUnsound of LocSetD.t
  | HrSound of LocSetD.t
  | HrExit
  | HrFallthrough

type flow_heurstic_type =
  Prog.t -> Loc.t -> AbsState.t -> Inst.t -> Mnemonic.t -> heuristic_result

let gen_bb_single (h : flow_heurstic_type) (p : Prog.t) (l : Loc.t)
    (a : AbsState.t) (i : Inst.t) (m : Mnemonic.t) : Loc.t option * Loc.t option
    =
  match h p l a i m with
  | HrUnsound s -> (None, None)
  | HrSound s -> (None, None)
  | HrExit -> (None, Some l)
  | HrFallthrough -> (Some (Prog.fallthru p l), Some l)

let gen_ujump_single (h : flow_heurstic_type) (p : Prog.t) (l : Loc.t)
    (a : AbsState.t) (i : Inst.t) (m : Mnemonic.t) : LocSetD.t option =
  match h p l a i m with
  | HrUnsound s -> Some s
  | HrSound s -> None
  | HrExit -> None
  | HrFallthrough -> None

let gen_jump_single (h : flow_heurstic_type) (p : Prog.t) (l : Loc.t)
    (a : AbsState.t) (i : Inst.t) (m : Mnemonic.t) : LocSetD.t =
  match h p l a i m with
  | HrUnsound s -> LocSetD.empty
  | HrSound s -> s
  | HrExit -> LocSetD.empty
  | HrFallthrough -> LocSetD.empty

let flow_heuristic_simple (p : Prog.t) (l : Loc.t) (a : AbsState.t) (i : Inst.t)
    (m : Mnemonic.t) : heuristic_result =
  match (i, m) with
  | Ijump v, "CALL" -> HrFallthrough
  | Ijump a, m ->
      if AddrMap.mem (Loc.to_addr a) p.externs then HrFallthrough
      else if
        String.starts_with ~prefix:"J" m || String.starts_with ~prefix:"M" m
      then HrSound (LocSetD.singleton a)
      else HrUnsound LocSetD.empty
  | Ijump_ind vn, "RET" -> HrExit
  | Ijump_ind vn, "CALL" -> HrFallthrough
  | Ijump_ind vn, _ -> (
      match AbsState.try_concretize_vn a vn 20 with
      | Some a ->
          HrSound
            (LocSetD.of_seq (Int64Set.to_seq a |> Seq.map (fun a -> (a, 0))))
      | None -> HrUnsound LocSetD.empty)
  | Icbranch (cn, a), _ -> HrSound (LocSetD.of_list [ a; Prog.fallthru p l ])
  | Iunimplemented, _ -> HrUnsound LocSetD.empty
  | _ -> HrSound (LocSetD.singleton (Prog.fallthru p l))

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
      FSAbsD.AbsLocMapD.mapi
        (fun l a ->
          match Prog.get_ins_full p l with
          | Some { ins = i; mnem = m } ->
              gen_ujump_single flow_heuristic_simple p l a i m
          | None -> None)
        a.post_state
    in
    let b =
      FSAbsD.AbsLocMapD.fold
        (fun l a (bens, bexs) ->
          let ins = Prog.get_ins_full p l in
          match ins with
          | Some { ins = i; mnem = m } ->
              let eno, exo = gen_bb_single flow_heuristic_simple p l a i m in
              ( Option.fold ~none:bens ~some:(fun l -> LocSetD.add l bens) eno,
                Option.fold ~none:bexs ~some:(fun l -> LocSetD.add l bexs) exo
              )
          | None -> (bens, bexs))
        a.post_state
        (LocSetD.empty, LocSetD.empty)
    in
    let j =
      FSAbsD.AbsLocMapD.mapi
        (fun l a ->
          match Prog.get_ins_full p l with
          | Some { ins = i; mnem = m } ->
              gen_jump_single flow_heuristic_simple p l a i m
          | None -> LocSetD.empty)
        a.post_state
    in
    (u, b, j)

  let post_contour_fs (p : Prog.t) (c : ContourD.t) (sj : JumpD.t)
      (a : FSAbsD.t) : FSAbsD.t =
    let newPost =
      FSAbsD.AbsLocMapD.filter_map
        (fun ls sa ->
          match Prog.get_ins p ls with
          | Some i ->
              let np = AbsState.post_single p ls sa i in
              FSAbsD.AbsLocMapD.find_opt ls a.post_state
              |> Option.map (fun a -> AbsState.join a np)
              |> Option.value ~default:np |> Option.some
          | None -> None)
        a.pre_state
    in
    let locHash = LocHashtbl.create 100 in
    JumpD.iter
      (fun ls s ->
        match (Prog.get_ins p ls, FSAbsD.AbsLocMapD.find_opt ls newPost) with
        | Some ins, Some ae ->
            LocSetD.iter
              (fun lf ->
                LocHashtbl.update locHash lf (fun v ->
                    match v with
                    | Some (af, lss) ->
                        Some
                          ( AbsState.join af
                              (AbsState.filter_single p ls lf ae ins),
                            LocSetD.add ls lss )
                    | None ->
                        Some
                          ( AbsState.filter_single p ls lf ae ins,
                            LocSetD.singleton ls )))
              s
        | _ -> ())
      sj;
    LocSetD.iter
      (fun l ->
        LocHashtbl.update locHash l (fun v ->
            Some (AbsState.top, LocSetD.empty)))
      (fst c.boundary_point);

    let newLocSeq =
      LocHashtbl.to_seq locHash
      |> Seq.filter_map (fun (l, (sa, lss)) ->
             if FSAbsD.AbsLocMapD.mem l a.pre_state then None else Some (l, sa))
    in

    let newPre =
      FSAbsD.AbsLocMapD.add_seq newLocSeq
        (FSAbsD.AbsLocMapD.mapi
           (fun ls s ->
             match LocHashtbl.find_opt locHash ls with
             | Some (sa, lss) ->
                 if LocSetD.exists (fun l -> fst ls < fst l) lss then
                   AbsState.widen s sa
                 else AbsState.join s sa
             | None -> s)
           a.pre_state)
    in

    { pre_state = newPre; post_state = newPost }

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
      abs_state =
        {
          pre_state = FSAbsD.AbsLocMapD.singleton l AbsState.top;
          post_state = FSAbsD.AbsLocMapD.empty;
        };
    }

  let rec a_fixpoint (p : Prog.t) (c : t) : t =
    let nc = join c (post p c) in
    if le nc c then c else a_fixpoint p nc

  let update_contour_single (p : Prog.t) (ca : t) (l : Loc.t) :
      ContourD.t * JumpD.t * Loc.t List.t =
    match
      ( Prog.get_ins_full p l,
        FSAbsD.AbsLocMapD.find_opt l ca.abs_state.post_state )
    with
    | Some { ins = i; mnem = m }, Some a ->
        let nuj =
          UJumpD.join_single_loc ca.analysis_contour.unsound_jump l
            (gen_ujump_single flow_heuristic_simple p l a i m)
        in
        let eno, exo = gen_bb_single flow_heuristic_simple p l a i m in
        let nb =
          Option.map
            (fun l ->
              BoundaryPointD.add_entry ca.analysis_contour.boundary_point l)
            eno
          |> Option.value ~default:ca.analysis_contour.boundary_point
        in
        let nb =
          Option.map (fun l -> BoundaryPointD.add_exit nb l) exo
          |> Option.value ~default:nb
        in
        let nj =
          JumpD.join_single_loc ca.sound_jump l
            (gen_jump_single flow_heuristic_simple p l a i m)
        in

        let ulist =
          JumpD.find_opt l nj |> Option.value ~default:LocSetD.empty
        in
        let ulist =
          Option.map (fun s -> LocSetD.add s ulist) eno
          |> Option.value ~default:ulist
          |> LocSetD.to_seq |> List.of_seq
        in
        ({ unsound_jump = nuj; boundary_point = nb }, nj, ulist)
    | _ -> (ca.analysis_contour, ca.sound_jump, [])

  let post_contour_single (p : Prog.t) (ca : t) (l : Loc.t) : FSAbsD.t * bool =
    let i =
      Prog.get_ins p l
      |> Option.value ~default:[%log raise (Invalid_argument "option is None")]
    in
    let sj = ca.sound_jump in
    let preds = JumpD.get_preds sj l in
    let na =
      List.filter_map
        (fun l ->
          FSAbsD.AbsLocMapD.find_opt l ca.abs_state.post_state
          |> Option.map (fun a -> (l, a)))
        preds
      |> List.fold_left
           (fun a (l, b) ->
             match a with
             | None -> Some (LocSetD.singleton l, b)
             | Some (lss, a) -> Some (LocSetD.add l lss, AbsState.join a b))
           Option.none
    in
    let na_pre =
      match
        ( na,
          FSAbsD.AbsLocMapD.find_opt l ca.abs_state.pre_state,
          LocSetD.mem l (fst ca.analysis_contour.boundary_point) )
      with
      | _, _, true -> AbsState.top
      | Some (lss, a), Some b, _ ->
          if LocSetD.exists (fun ls -> fst l < fst ls) lss then
            AbsState.widen b a
          else AbsState.join b a
      | Some (lss, a), None, _ -> a
      | None, Some a, _ -> a
      | None, None, false -> [%log raise (Failure "Assertion failed: find_opt")]
    in
    let abs_1 : FSAbsD.t =
      {
        pre_state = FSAbsD.AbsLocMapD.add l na_pre ca.abs_state.pre_state;
        post_state = ca.abs_state.post_state;
      }
    in
    let np = AbsState.post_single p l na_pre i in
    match FSAbsD.AbsLocMapD.find_opt l ca.abs_state.post_state with
    | Some a ->
        if AbsState.le a np then (abs_1, false)
        else (FSAbsD.join_single_post abs_1 l np, true)
    | None -> (FSAbsD.join_single_post abs_1 l np, true)

  let post_worklist (p : Prog.t) (c : t) (l : Loc.t) : t * Loc.t List.t =
    let na, propagated = post_contour_single p c l in
    let nac, nsj, newList =
      if propagated then
        update_contour_single p
          {
            analysis_contour = c.analysis_contour;
            sound_jump = c.sound_jump;
            abs_state = na;
          }
          l
      else (c.analysis_contour, c.sound_jump, [])
    in
    let nc = { analysis_contour = nac; sound_jump = nsj; abs_state = na } in
    (nc, newList)

  let rec a_fixpoint_worklist (p : Prog.t) (c : t) (ls : Loc.t List.t) : t =
    match ls with
    | [] -> c
    | l :: ls ->
        let nc, newLs = post_worklist p c l in
        a_fixpoint_worklist p nc
          (ls @ List.filter (fun l -> not (List.mem l ls)) newLs)

  let follow_flow (p : Prog.t) (e : Addr.t) : t =
    a_fixpoint_worklist p (init p (e, 0)) ((e, 0) :: [])
end

module Mutable = struct
  type __ = {
    analysis_contour : ContourD_Mut.t;
    sound_jump : JumpD_Mut.t;
    abs_state : FSAbsD_Mut.t;
  }

  include
    TripleD.MakeJoinSemiLattice_Mut_Record (ContourD_Mut) (JumpD_Mut)
      (FSAbsD_Mut)
      (struct
        type t = __

        let get_fst x = x.analysis_contour
        let get_snd x = x.sound_jump
        let get_trd x = x.abs_state
      end)

  let update_contour (p : Prog.t) (ca : t) (l : Loc.t) : Loc.t List.t =
    match
      ( Prog.get_ins_full p l,
        FSAbsD_Mut.AbsLocMapD.find_opt l ca.abs_state.post_state )
    with
    | Some { ins = i; mnem = m }, Some a ->
        UJumpD_Mut.join_single_loc ca.analysis_contour.unsound_jump l
          (gen_ujump_single flow_heuristic_simple p l a i m);
        let eno, exo = gen_bb_single flow_heuristic_simple p l a i m in
        Option.iter
          (fun l ->
            BoundaryPointD_Mut.add_entry ca.analysis_contour.boundary_point l)
          eno;
        Option.iter
          (fun l ->
            BoundaryPointD_Mut.add_exit ca.analysis_contour.boundary_point l)
          exo;
        JumpD_Mut.join_single_loc ca.sound_jump l
          (gen_jump_single flow_heuristic_simple p l a i m);
        JumpD_Mut.find_opt l ca.sound_jump
        |> Option.value ~default:LocSetD.empty
        |> LocSetD.to_seq |> List.of_seq
    | _ -> []

  let post_contour_fs (p : Prog.t) (ca : t) (l : Loc.t) : bool =
    let sj = ca.sound_jump in
    let preds = JumpD_Mut.get_preds sj l in
    List.filter_map
      (fun l -> FSAbsD_Mut.AbsLocMapD.find_opt l ca.abs_state.pre_state)
      preds
    |> List.iter (fun a -> FSAbsD_Mut.join_single_pre ca.abs_state l a);
    match
      (FSAbsD_Mut.AbsLocMapD.find_opt l ca.abs_state.pre_state, Prog.get_ins p l)
    with
    | Some a, Some i -> (
        let np = AbsState.post_single p l a i in
        match FSAbsD_Mut.AbsLocMapD.find_opt l ca.abs_state.post_state with
        | Some a ->
            if AbsState.le a np then false
            else (
              FSAbsD_Mut.join_single_post ca.abs_state l np;
              true)
        | None ->
            FSAbsD_Mut.join_single_post ca.abs_state l np;
            true)
    | _ -> [%log raise (Failure "Assertion failed: find_opt")]

  let post (p : Prog.t) (c : t) (l : Loc.t) : Loc.t List.t =
    let propagate = post_contour_fs p c l in
    let newList = update_contour p c l in
    if propagate then newList else []

  let init (p : Prog.t) (l : Loc.t) : t =
    {
      analysis_contour =
        {
          unsound_jump = UJumpD_Mut.create_empty ();
          boundary_point = (LocSetD_Mut.singleton l, LocSetD_Mut.create_empty ());
        };
      sound_jump = JumpD_Mut.create_empty ();
      abs_state =
        {
          pre_state = FSAbsD_Mut.AbsLocMapD.singleton l AbsState.top;
          post_state = FSAbsD_Mut.AbsLocMapD.create_empty ();
        };
    }

  let rec a_fixpoint (p : Prog.t) (c : t) (ls : Loc.t List.t) : t =
    match ls with
    | [] -> c
    | l :: ls ->
        let newLs = post p c l in
        a_fixpoint p c (ls @ List.filter (fun l -> not (List.mem l ls)) newLs)

  let follow_flow (p : Prog.t) (e : Addr.t) : t =
    a_fixpoint p (init p (e, 0)) ((e, 0) :: [])
end
