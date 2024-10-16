open Common
open Basic_domain
open Value_domain
open Syn

module Immutable = struct
  type __ = { accesses : AccessD.t; states : FSAbsD.t }

  include
    TupleD.MakeJoinSemiLattice_Record (AccessD) (FSAbsD)
      (struct
        type t = __

        let get_fst x = x.accesses
        let get_snd x = x.states
        let make x y = { accesses = x; states = y }
      end)

  let check_sp_fixed (a : t) (sp_num : Int32.t) : bool =
    let epre =
      FSAbsD.AbsLocMapD.exists
        (fun _ (a : AbsState.t) ->
          AbsVal.le Top (ARegFile.get a.regs (Register sp_num)))
        a.states.pre_state
    in
    let epost =
      FSAbsD.AbsLocMapD.exists
        (fun _ (a : AbsState.t) ->
          AbsVal.le Top (ARegFile.get a.regs (Register sp_num)))
        a.states.post_state
    in
    not (epre || epost)

  let init (f : Func.t) (sp_num : Int32.t) (fp_num : Int32.t) : t =
    {
      states =
        {
          pre_state =
            FSAbsD.AbsLocMapD.singleton f.entry
              {
                AbsState.regs =
                  ARegFile.TopHoleMap
                    (RegIdMap.singleton (Register sp_num)
                       (AbsVal.of_sp_offset 0L));
                stack = AStack.TopHoleMap Byte8Map.empty;
              };
          post_state = FSAbsD.AbsLocMapD.empty;
        };
      accesses = AccessD.Fin (Int64SetD.singleton 0L);
    }

  let post_single_block (f : Func.t) (bb : Block.t) (ca : t) (sp_num : Int32.t)
      (fp_num : Int32.t) : t * bool =
    let preds = Func.get_preds f bb in
    let na =
      List.filter_map
        (fun (l : Block.t) ->
          FSAbsD.AbsLocMapD.find_opt l.loc ca.states.post_state
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
      match (na, FSAbsD.AbsLocMapD.find_opt bb.loc ca.states.pre_state) with
      | Some (lss, a), Some b ->
          if
            LocSetD.exists (fun ls -> Loc.get_addr bb.loc < Loc.get_addr ls) lss
          then AbsState.widen b a
          else AbsState.join b a
      | Some (lss, a), None -> a
      | None, Some a -> a
      | None, None -> [%log raise (Failure "Assertion failed: find_opt")]
    in
    let abs_1 : FSAbsD.t =
      {
        pre_state = FSAbsD.AbsLocMapD.add bb.loc na_pre ca.states.pre_state;
        post_state = ca.states.post_state;
      }
    in
    let naccess, np = AbsState.post_single_block bb na_pre sp_num fp_num in
    match FSAbsD.AbsLocMapD.find_opt bb.loc ca.states.post_state with
    | Some a ->
        if AbsState.le np a then
          ({ accesses = ca.accesses; states = abs_1 }, false)
        else
          ( {
              accesses = AccessD.join ca.accesses naccess;
              states = FSAbsD.join_single_post abs_1 bb.loc np;
            },
            true )
    | None ->
        ( {
            accesses = AccessD.join ca.accesses naccess;
            states = FSAbsD.join_single_post abs_1 bb.loc np;
          },
          true )

  let post_worklist (f : Func.t) (c : t) (l : Loc.t) (sp_num : Int32.t)
      (fp_num : Int32.t) : t * Loc.t List.t =
    let bb =
      (Func.get_bb f l |> Option.map Fun.const
      |> Option.value ~default:(fun () ->
             [%log
               raise
                 (Invalid_argument
                    (Format.asprintf "%s:%a option is None"
                       (Option.value (Func.nameo f) ~default:"noname")
                       Loc.pp l))]))
        ()
    in
    let na, propagated = post_single_block f bb c sp_num fp_num in
    if propagated then (na, Block.succ bb) else (na, [])

  let rec a_fixpoint_worklist (f : Func.t) (c : t) (ls : Loc.t List.t)
      (sp_num : Int32.t) (fp_num : Int32.t) : t =
    match ls with
    | [] -> c
    | l :: ls ->
        let nc, newLs = post_worklist f c l sp_num fp_num in
        a_fixpoint_worklist f nc
          (ls @ List.filter (fun l -> not (List.mem l ls)) newLs)
          sp_num fp_num

  let analyze (f : Func.t) (sp_num : Int32.t) (fp_num : Int32.t) : t =
    [%log
      debug "start analysis %s" (Option.value (Func.nameo f) ~default:"noname")];
    a_fixpoint_worklist f (init f sp_num fp_num) (f.entry :: []) sp_num fp_num
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
