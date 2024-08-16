open Common

let check_error (a1 : ('a, StopEvent.t) Result.t)
    (a2 : ('b, StopEvent.t) Result.t) (a3 : ('c, StopEvent.t) Result.t) :
    ('a * 'b * 'c, StopEvent.t) Result.t =
  match (a1, a2, a3) with
  | Ok a1, Ok a2, Ok a3 -> Ok (a1, a2, a3)
  | Error _, Ok _, _ -> Error (FailStop "No backward sim (ASIR)")
  | _, Error _, Ok _ -> Error (FailStop "No backward sim (IOIR)")
  | _, _, Error _ -> Error NormalStop

let check_reg (r1 : RegId.t_full) (r2 : RegId.t_full) (r3 : RegId.t_full) :
    (Unit.t, StopEvent.t) Result.t =
  if RegId.compare r1.id r2.id = 0 && RegId.compare r2.id r3.id = 0 then
    if
      r1.width = r2.width && r2.width = r3.width && r1.offset = r2.offset
      && r2.offset = r3.offset
    then Ok ()
    else Error (FailStop "Not same reg")
  else Error (FailStop "Not same reg")

let check_val (v1 : FGIR.Sem.Value.t) (v2 : ASIR.Sem.Value.t)
    (v3 : IOIR.Sem.Value.t) (sps : FGIR.Sem.Value.t Common.FuncTimestampMap.t) :
    (Unit.t, StopEvent.t) Result.t =
  match (v1, v2, v3) with
  | v, Num v', Num v'' ->
      if NumericValue.subsume v v' && NumericValue.subsume v' v'' then Ok ()
      else
        Error
          (FailStop
             (Format.asprintf "Not same val: %a %a %a" NumericValue.pp v
                NumericValue.pp v' NumericValue.pp v''))
  | v, NonNum (SP p), NonNum (SP p') ->
      if
        p.func = p'.func && p.timestamp = p'.timestamp && p.offset = p'.offset
        && p.multiplier = p'.multiplier
      then
        match FuncTimestampMap.find_opt (p.func, p.timestamp) sps with
        | Some vsp ->
            let* fv = NumericValue.value_64 v |> StopEvent.of_str_res in
            let* gv = NumericValue.value_64 vsp |> StopEvent.of_str_res in
            if Int64.add (Int64.mul gv p.multiplier) p.offset = fv then Ok ()
            else Error (FailStop "Not same SP Val")
        | None -> Error (FailStop "Not have FGIR SP Val")
      else Error (FailStop "Not same SP offset")
  | _ -> Ok ()

let check_action (a1 : FGIR.Sem.Action.t) (a2 : ASIR.Sem.Action.t)
    (a3 : IOIR.Sem.Action.t)
    ((vsp, ts', ts'') : FGIR.Sem.Value.t * Int64.t * Int64.t)
    (sps : FGIR.Sem.Value.t Common.FuncTimestampMap.t) :
    (FGIR.Sem.Value.t Common.FuncTimestampMap.t, StopEvent.t) Result.t =
  match (a1, a2, a3) with
  | ( ExternCall (name, values, args, ft),
      ExternCall (name', values', args', ft'),
      ExternCall (name'', values'', args'', ft'') ) ->
      Ok sps
  | ( StoreAction (Load (r, p, v), lo),
      StoreAction (Load (r', p', v'), lo'),
      StoreAction (Load (r'', p'', v''), lo'') ) ->
      let* _ =
        check_reg r r' r''
        |> Fun.flip StopEvent.map_error (fun s ->
               Format.asprintf "Load-1: %s" s)
      in
      let* _ =
        check_val p p' p'' sps
        |> Fun.flip StopEvent.map_error (fun s ->
               Format.asprintf "Load-2: %s" s)
      in
      let* _ =
        check_val v v' v'' sps
        |> Fun.flip StopEvent.map_error (fun s ->
               Format.asprintf "Load-3: %s" s)
      in
      Ok sps
  | ( StoreAction (Store (p, v), lo),
      StoreAction (Store (p', v'), lo'),
      StoreAction (Store (p'', v''), lo'') ) ->
      let* _ =
        check_val p p' p'' sps
        |> Fun.flip StopEvent.map_error (fun s ->
               Format.asprintf "Store-1: %s" s)
      in
      let* _ =
        check_val v v' v'' sps
        |> Fun.flip StopEvent.map_error (fun s ->
               Format.asprintf "Store-2: %s" s)
      in
      Ok sps
  | ( StoreAction (Assign (r, v), lo),
      StoreAction (Assign (r', v'), lo'),
      StoreAction (Assign (r'', v''), lo'') ) ->
      let* _ =
        check_reg r r' r''
        |> Fun.flip StopEvent.map_error (fun s ->
               Format.asprintf "Assign-1: %s" s)
      in
      let* _ =
        check_val v v' v'' sps
        |> Fun.flip StopEvent.map_error (fun s ->
               Format.asprintf "Assign-2: %s" s)
      in
      Ok sps
  | StoreAction (Nop, lo), StoreAction (Nop, lo'), StoreAction (Nop, lo'') ->
      Ok sps
  | Jmp l, Jmp l', Jmp l'' ->
      if Loc.compare l l' = 0 && Loc.compare l' l'' = 0 then Ok sps
      else Error (FailStop "Not same jump target")
  | Call sc, Call sc', Call sc'' ->
      let t = FGIR.Sem.SCallTarget.get_target (FGIR.Sem.SCall.get_target sc) in
      let t' =
        ASIR.Sem.SCallTarget.get_target (ASIR.Sem.SCall.get_target sc')
      in
      let t'' =
        IOIR.Sem.SCallTarget.get_target (IOIR.Sem.SCall.get_target sc'')
      in
      if
        Loc.compare t t' = 0
        && Loc.compare t' t'' = 0
        && Int64.compare ts' ts'' = 0
      then Ok (FuncTimestampMap.add (t, Int64.succ ts') vsp sps)
      else Error (FailStop "Not same call target")
  | Ret sr, Ret sr', Ret sr'' -> Ok sps
  | TailCall st, TailCall st', TailCall st'' -> Ok sps
  | _ -> Error (FailStop "Not same action")

let action_all a1 l1 l1_state a2 l2 l2_state a3 l3 l3_state =
  match (a1, a2, a3) with
  | FGIR.Sem.Action.ExternCall (name, sides, args, _), _, _ -> (
      match World.Environment.request_call_opt name args with
      | Some retv ->
          ( FGIR.Interp.action_with_computed_extern l1 l1_state a1 sides retv,
            ASIR.Interp.action_with_computed_extern l2 l2_state a2 sides retv,
            IOIR.Interp.action_with_computed_extern l3 l3_state a3 sides retv )
      | None ->
          ( StopEvent.NormalStop |> Result.error,
            StopEvent.NormalStop |> Result.error,
            StopEvent.NormalStop |> Result.error ))
  | _ ->
      ( FGIR.Interp.action l1 l1_state a1,
        ASIR.Interp.action l2 l2_state a2,
        IOIR.Interp.action l3 l3_state a3 )

let run (l1 : FGIR.Syn.Prog.t) (l2 : ASIR.Syn.Prog.t) (l3 : IOIR.Syn.Prog.t)
    (addr : Byte8.t) : (Unit.t, StopEvent.t) Result.t =
  let l1_state = FGIR.Init.from_signature l1 [] addr in
  (* TODO: args *)
  let l2_state = ASIR.Init.from_signature l2 [] addr in
  let l3_state = IOIR.Init.from_signature l3 [] addr in
  let sps =
    FuncTimestampMap.singleton
      (Loc.of_addr addr, 0L)
      (FGIR.Sem.Value.of_int64 0x7FFFFFFFC000L 8l)
  in
  let rec aux (l1_state : FGIR.Sem.State.t) (l2_state : ASIR.Sem.State.t)
      (l3_state : IOIR.Sem.State.t)
      (sps : FGIR.Sem.Value.t Common.FuncTimestampMap.t) :
      (Unit.t, StopEvent.t) Result.t =
    let a1 = FGIR.Interp.step l1 l1_state in
    let a2 = ASIR.Interp.step l2 l2_state in
    let a3 = IOIR.Interp.step l3 l3_state in
    let* a1, a2, a3 =
      check_error a1 a2 a3
      |> Fun.flip StopEvent.map_error (fun s ->
             Format.asprintf "Error-1: %a %s" Loc.pp
               (FGIR.Sem.Cont.get_loc (FGIR.Sem.State.get_cont l1_state))
               s)
    in
    let* nsps =
      check_action a1 a2 a3
        ( FGIR.Sem.Store.get_reg
            (FGIR.Sem.State.get_store l1_state)
            { id = RegId.Register 32l; offset = 0l; width = 8l },
          ASIR.Sem.State.get_timestamp l2_state,
          IOIR.Sem.State.get_timestamp l3_state )
        sps
      |> Fun.flip StopEvent.map_error (fun s ->
             Format.asprintf "Action: %a %s" Loc.pp
               (FGIR.Sem.Cont.get_loc (FGIR.Sem.State.get_cont l1_state))
               s)
    in
    let nl1, nl2, nl3 =
      action_all a1 l1 l1_state a2 l2 l2_state a3 l3 l3_state
    in
    let* nl1, nl2, nl3 =
      check_error nl1 nl2 nl3
      |> Fun.flip StopEvent.map_error (fun s ->
             Format.asprintf "Error-2: %a %s" Loc.pp
               (FGIR.Sem.Cont.get_loc (FGIR.Sem.State.get_cont l1_state))
               s)
    in
    aux nl1 nl2 nl3 nsps
  in
  aux l1_state l2_state l3_state sps
