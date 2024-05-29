open StdlibExt
open Notation
open Common
open Sem

let step_call_internal (s : State.t) (p : Prog.t)
    ({ target = { target = calln; attr }; attr = (); fallthrough } : SCall.t) :
    (Store.t * Stack.elem_t, StopEvent.t) Result.t =
  let* currf = State.get_current_function s p |> StopEvent.of_str_res in
  let* f = State.get_func_from p calln |> StopEvent.of_str_res in

  (* TODO: think ind copydepth
     let* _ =
       if snd f.sp_boundary <= copydepth then Ok ()
       else Error "jcall_ind: copydepth not match"
     in
  *)
  (s.sto, (s.cursor, fallthrough)) |> Result.ok

let step_ret (s : State.t) (p : Prog.t) ({ attr } : SRet.t) (calln, retn') :
    (Store.t, StopEvent.t) Result.t =
  let* attr = Value.try_loc attr |> StopEvent.of_str_res in
  if Loc.compare attr retn' <> 0 then
    StopEvent.FailStop "Retaddr not matched" |> Result.error
  else Ok s.sto

let action_JC = State.mk_action_JC step_call_internal
let action_JR = State.mk_action_JR step_ret

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) (curr : Cursor.t) :
    (StoreAction.t, String.t) Result.t =
  Inst.fold (Store.step_ILS s) (Store.step_IA s) (Store.step_IN s) ins

let step_jmp (p : Prog.t) (jmp : Jmp.t) (s : State.t) :
    (Action.t, String.t) Result.t =
  match jmp with
  | JI ji -> State.step_JI p s ji
  | JC jc -> (
      let* sc = SCall.eval s.sto jc in
      match Byte8Map.find_opt (Loc.get_addr sc.target.target) p.externs with
      | None -> Action.call sc |> Result.ok
      | Some name -> State.step_call_external p s.sto name sc.fallthrough)
  | JR jr ->
      let* sr = SRet.eval s.sto jr in
      Action.ret sr |> Result.ok
  | JT _ | JswitchStop _ -> Error "unimplemented jump"

let step (p : Prog.t) (s : State.t) : (Action.t, StopEvent.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } -> step_jmp p jmp.jmp s |> StopEvent.of_str_res
  | { remaining = { ins = Third _; _ } :: []; jmp } ->
      step_jmp p jmp.jmp s |> StopEvent.of_str_res
  | { remaining = i :: []; jmp = { jmp = JI (JIntra.Jfallthrough l) } } ->
      let* a = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
      Action.of_store a (Some l) |> Result.ok
  | { remaining = i :: res; jmp } ->
      if List.is_empty res then
        StopEvent.FailStop "Not possible inst" |> Result.error
      else
        let* a = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
        Action.of_store a None |> Result.ok

let action (p : Prog.t) (s : State.t) (a : Action.t) :
    (State.t, StopEvent.t) Result.t =
  match a with
  | StoreAction (a, lo) -> (
      let* sto = Store.action s.sto a |> StopEvent.of_str_res in
      match (lo, s.cont) with
      | None, { remaining = _ :: res; jmp } ->
          Ok { s with sto; cont = { remaining = res; jmp } }
      | Some l, _ ->
          let* cont =
            Cont.of_loc p (State.get_func_loc s) l |> StopEvent.of_str_res
          in
          Ok { s with sto; cont }
      | _ -> StopEvent.FailStop "Not possible inst" |> Result.error)
  | Jmp l -> State.action_jmp p s l |> StopEvent.of_str_res
  | ExternCall (name, values, args, ft) -> (
      match World.Environment.request_call_opt name args with
      | Some (sides, retv) -> State.action_extern p s values sides retv ft
      | None -> StopEvent.NormalStop |> Result.error)
  | Call sc -> action_JC p s sc
  | TailCall st -> StopEvent.FailStop "unimplemented jump" |> Result.error
  | Ret sr -> action_JR p s sr

let action_with_computed_extern (p : Prog.t) (s : State.t) (a : Action.t)
    (sides : (Int.t * Interop.t) List.t) (retv : Interop.t) :
    (State.t, StopEvent.t) Result.t =
  match a with
  | StoreAction (a, lo) -> (
      let* sto = Store.action s.sto a |> StopEvent.of_str_res in
      match (lo, s.cont) with
      | None, { remaining = _ :: res; jmp } ->
          Ok { s with sto; cont = { remaining = res; jmp } }
      | Some l, _ ->
          let* cont =
            Cont.of_loc p (State.get_func_loc s) l |> StopEvent.of_str_res
          in
          Ok { s with sto; cont }
      | _ -> StopEvent.FailStop "Not possible inst" |> Result.error)
  | Jmp l -> State.action_jmp p s l |> StopEvent.of_str_res
  | ExternCall (name, values, args, ft) ->
      State.action_extern p s values sides retv ft
  | Call sc -> action_JC p s sc
  | TailCall st -> StopEvent.FailStop "unimplemented jump" |> Result.error
  | Ret sr -> action_JR p s sr

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  let* a =
    step p s |> Fun.flip StopEvent.add_loc (Cont.get_loc (State.get_cont s))
  in
  let* s' =
    action p s a |> Fun.flip StopEvent.add_loc (Cont.get_loc (State.get_cont s))
  in
  interp p s'
