open StdlibExt
open Notation
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
    (v3 : IOIR.Sem.Value.t) : (Unit.t, StopEvent.t) Result.t =
  match (v1, v2, v3) with
  | v, Num v', Num v'' ->
      if v = v' && v' = v'' then Ok ()
      else
        Error
          (FailStop
             (Format.asprintf "Not same val: %a %a %a" NumericValue.pp v
                NumericValue.pp v' NumericValue.pp v''))
  | _ -> Ok () (* TODO: check abstract values *)

let check_action (a1 : FGIR.Sem.Action.t) (a2 : ASIR.Sem.Action.t)
    (a3 : IOIR.Sem.Action.t) : (Unit.t, StopEvent.t) Result.t =
  match (a1, a2, a3) with
  | ( ExternCall (name, values, args, ft),
      ExternCall (name', values', args', ft'),
      ExternCall (name'', values'', args'', ft'') ) ->
      Ok ()
  | ( StoreAction (Load (r, p, v), lo),
      StoreAction (Load (r', p', v'), lo'),
      StoreAction (Load (r'', p'', v''), lo'') ) ->
      let* _ = check_reg r r' r'' in
      let* _ = check_val p p' p'' in
      let* _ = check_val v v' v'' in
      Ok ()
  | ( StoreAction (Store (p, v), lo),
      StoreAction (Store (p', v'), lo'),
      StoreAction (Store (p'', v''), lo'') ) ->
      let* _ = check_val p p' p'' in
      let* _ = check_val v v' v'' in
      Ok ()
  | ( StoreAction (Assign (r, v), lo),
      StoreAction (Assign (r', v'), lo'),
      StoreAction (Assign (r'', v''), lo'') ) ->
      let* _ = check_reg r r' r'' in
      let* _ = check_val v v' v'' in
      Ok ()
  | StoreAction (Nop, lo), StoreAction (Nop, lo'), StoreAction (Nop, lo'') ->
      Ok ()
  | Jmp l, Jmp l', Jmp l'' -> Ok ()
  | Call sc, Call sc', Call sc'' -> Ok ()
  | Ret sr, Ret sr', Ret sr'' -> Ok ()
  | TailCall st, TailCall st', TailCall st'' -> Ok ()
  | _ -> Error (FailStop "Not same action")

let action_all a1 l1 l1_state a2 l2 l2_state a3 l3 l3_state =
  match (a1, a2, a3) with
  | FGIR.Sem.Action.ExternCall (name, _, args, _), _, _ -> (
      match World.Environment.request_call_opt name args with
      | Some (sides, retv) ->
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

let run (l1 : FGIR.Prog.t) (l2 : ASIR.Prog.t) (l3 : IOIR.Prog.t) (addr : Addr.t)
    : (Unit.t, StopEvent.t) Result.t =
  let l1_state = FGIR.Init.from_signature l1 addr in
  let l2_state = ASIR.Init.from_signature l2 addr in
  let l3_state = IOIR.Init.from_signature l3 addr in
  let rec aux (l1_state : FGIR.Sem.State.t) (l2_state : ASIR.Sem.State.t)
      (l3_state : IOIR.Sem.State.t) : (Unit.t, StopEvent.t) Result.t =
    let a1 = FGIR.Interp.step l1 l1_state in
    let a2 = ASIR.Interp.step l2 l2_state in
    let a3 = IOIR.Interp.step l3 l3_state in
    let* a1, a2, a3 = check_error a1 a2 a3 in
    let* _ = check_action a1 a2 a3 in
    let nl1, nl2, nl3 =
      action_all a1 l1 l1_state a2 l2 l2_state a3 l3 l3_state
    in
    let* nl1, nl2, nl3 = check_error nl1 nl2 nl3 in
    aux nl1 nl2 nl3
  in
  aux l1_state l2_state l3_state
