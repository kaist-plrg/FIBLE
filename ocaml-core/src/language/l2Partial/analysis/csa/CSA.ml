open Common
open Basic_domain
open Value_domain

module Immutable = struct
  type t = FSAbsD.t

  let init (f : Func.t) : t =
    {
      FSAbsD.pre_state = FSAbsD.AbsLocMapD.singleton f.entry AbsState.init;
      post_state = FSAbsD.AbsLocMapD.empty;
    }

  let post_single_block (f : Func.t) (bb : Block.t) (ca : t) : t * bool =
    let preds = Func.get_preds f bb in
    let na =
      List.filter_map
        (fun (l : Block.t) ->
          FSAbsD.AbsLocMapD.find_opt l.loc ca.post_state
          |> Option.map (fun a -> (l.loc, a)))
        preds
      |> List.fold_left
           (fun a (l, b) ->
             match a with
             | None -> Some (LocSetD.singleton l, b)
             | Some (lss, a) -> Some (LocSetD.add l lss, AbsState.join a b))
           Option.none
    in
    let na_pre =
      match (na, FSAbsD.AbsLocMapD.find_opt bb.loc ca.pre_state) with
      | Some (lss, a), Some b ->
          if LocSetD.exists (fun ls -> fst bb.loc < fst ls) lss then
            AbsState.widen b a
          else AbsState.join b a
      | Some (lss, a), None -> a
      | None, Some a -> a
      | None, None -> [%log raise (Failure "Assertion failed: find_opt")]
    in
    let abs_1 : FSAbsD.t =
      {
        pre_state = FSAbsD.AbsLocMapD.add bb.loc na_pre ca.pre_state;
        post_state = ca.post_state;
      }
    in
    let np = AbsState.post_single_block bb na_pre in
    match FSAbsD.AbsLocMapD.find_opt bb.loc ca.post_state with
    | Some a ->
        if AbsState.le a np then (abs_1, false)
        else (FSAbsD.join_single_post abs_1 bb.loc np, true)
    | None -> (FSAbsD.join_single_post abs_1 bb.loc np, true)

  let post_worklist (f : Func.t) (c : t) (l : Loc.t) : t * Loc.t List.t =
    let bb =
      (Func.get_bb f l |> Option.map Fun.const
      |> Option.value ~default:(fun () ->
             [%log raise (Invalid_argument "option is None")]))
        ()
    in
    let na, propagated = post_single_block f bb c in
    if propagated then (na, Block.succ bb) else (na, [])

  let rec a_fixpoint_worklist (f : Func.t) (c : t) (ls : Loc.t List.t) : t =
    match ls with
    | [] -> c
    | l :: ls ->
        let nc, newLs = post_worklist f c l in
        a_fixpoint_worklist f nc
          (ls @ List.filter (fun l -> not (List.mem l ls)) newLs)

  let analyze (f : Func.t) : t = a_fixpoint_worklist f (init f) (f.entry :: [])
  (*
  let rec a_fixpoint_worklist_prog (p : Prog.t) (c : t) (ls : Loc.t List.t)
    (sp_num : int64) : t =
  match ls with
  | [] -> c
  | l :: ls ->
      let nc, newLs = post_worklist f c l sp_num in
      a_fixpoint_worklist f nc
        (ls @ List.filter (fun l -> not (List.mem l ls)) newLs)
        sp_num
  
  let analyze_prog (p: Prog.t) (sp_num: int64): t =
    a_fixpoint_worklist_prog p (init_prog p sp_num) (Prog.get_funcs p) sp_num
    *)
end
