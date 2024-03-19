open StdlibExt
open Notation
open Common
open Sem

let step_call_internal (s : State.t) (p : Prog.t)
    ({
       target = { target = calln; attr };
       attr = { reserved_stack = copydepth; sp_diff };
       fallthrough;
     } :
      SCall.t) : (Store.t * Stack.elem_t, StopEvent.t) Result.t =
  let* currf = State.get_current_function s p |> StopEvent.of_str_res in
  let* f = State.get_func_from p calln |> StopEvent.of_str_res in
  let* _ =
    if f.sp_diff = sp_diff then Ok ()
    else Error (StopEvent.FailStop "jcall_ind: spdiff not match")
  in

  (* TODO: think ind copydepth
     let* _ =
       if snd f.sp_boundary <= copydepth then Ok ()
       else Error "jcall_ind: copydepth not match"
     in
  *)
  let* saved_sp =
    Store.build_saved_sp s.sto p sp_diff |> StopEvent.of_str_res
  in
  let ndepth =
    match attr with Some () -> copydepth | None -> snd f.sp_boundary
  in
  let* nlocal =
    Store.build_local_frame s.sto p f.sp_boundary ndepth |> StopEvent.of_str_res
  in
  let regs =
    RegFile.add_reg s.sto.regs
      { id = RegId.Register p.sp_num; offset = 0l; width = 8l }
      (Value.sp
         {
           func = calln;
           timestamp = TimeStamp.succ s.timestamp;
           multiplier = 1L;
           offset = 0L;
         })
  in
  let sto =
    {
      s.sto with
      regs;
      local =
        s.sto.local
        |> LocalMemory.add (calln, TimeStamp.succ s.timestamp) nlocal;
    }
  in
  (sto, (s.cursor, saved_sp, fallthrough)) |> Result.ok

let step_ret (s : State.t) (p : Prog.t) ({ attr } : SRet.t)
    (calln, sp_saved, retn') : (Store.t, StopEvent.t) Result.t =
  Ok
    {
      s.sto with
      regs =
        RegFile.add_reg s.sto.regs
          { id = RegId.Register p.sp_num; offset = 0l; width = 8l }
          sp_saved;
    }

let action_JC = State.mk_action_JC step_call_internal
let action_JR = State.mk_action_JR step_ret

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) (curr : Cursor.t) :
    (StoreAction.t, String.t) Result.t =
  match ins with
  | IA i -> Store.step_IA s i
  | ILS i -> Store.step_ILS s i
  | ISLS i -> Store.step_ISLS s i curr
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
  | JT _ -> Error "unimplemented jump"

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
  let* a = step p s in
  let* s' = action p s a in
  interp p s'
