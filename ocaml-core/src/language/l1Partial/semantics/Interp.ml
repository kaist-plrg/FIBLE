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
  if Loc.compare (Value.to_loc attr) retn' != 0 then
    StopEvent.FailStop "Retaddr not matched" |> Result.error
  else Ok s.sto

let action_JC = State.mk_action_JC step_call_internal
let action_JR = State.mk_action_JR step_ret

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) (curr : Cursor.t) :
    (StoreAction.t, String.t) Result.t =
  match ins with
  | IA i -> Store.step_IA s i
  | ILS i -> Store.step_ILS s i
  | IN i -> Store.step_IN s i

let step_jmp (p : Prog.t) (jmp : Jmp.t) (s : State.t) :
    (Action.t, String.t) Result.t =
  match jmp with
  | JI ji -> State.step_JI s p ji
  | JC jc -> (
      let* sc = SCall.eval s.sto jc in
      match AddrMap.find_opt (Loc.to_addr sc.target.target) p.externs with
      | None -> Action.call sc |> Result.ok
      | Some name -> State.step_call_external p s.sto name sc.fallthrough)
  | JR jr ->
      let* sr = SRet.eval s.sto jr in
      Action.ret sr |> Result.ok
  | JT _ | JswitchStop _ -> Error "unimplemented jump"

let step (p : Prog.t) (s : State.t) : (Action.t, StopEvent.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } ->
      step_jmp p jmp.jmp s |> StopEvent.of_str_res
      |> Fun.flip StopEvent.add_loc jmp.loc
  | { remaining = { ins = IN _; _ } :: []; jmp } ->
      step_jmp p jmp.jmp s |> StopEvent.of_str_res
      |> Fun.flip StopEvent.add_loc jmp.loc
  | { remaining = i :: []; jmp = { jmp = JI (JIntra.Jfallthrough l) } } ->
      let* a = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
      Action.of_store a (Some l) |> Result.ok
      |> Fun.flip StopEvent.add_loc i.loc
  | { remaining = i :: res; jmp } ->
      if List.is_empty res then
        StopEvent.FailStop "Not possible inst" |> Result.error
      else
        let* a = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
        Action.of_store a None |> Result.ok |> Fun.flip StopEvent.add_loc i.loc

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
  | ExternCall (name, values, args, ft) -> Ok s (* TODO *)
  | Call sc -> action_JC p s sc
  | TailCall st -> StopEvent.FailStop "unimplemented jump" |> Result.error
  | Ret sr -> action_JR p s sr

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  let* a = step p s in
  let* s' = action p s a in
  interp p s'
