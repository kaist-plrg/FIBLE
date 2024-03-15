open StdlibExt
open Common
open Notation
open Sem

let build_inputs_from_sig (f : Func.t) : VarNode.t List.t =
  f.inputs
  |> List.map (fun v -> VarNode.Register { id = v; offset = 0l; width = 8l })

let reg_build_from_input (sto : Store.t) (inputs : VarNode.t List.t)
    (f : Func.t) : RegFile.t =
  List.fold_left
    (fun r (i, v) ->
      RegFile.add_reg r
        { id = i; offset = 0l; width = 8l }
        (Store.eval_vn sto v |> Result.get_ok))
    (RegFile.of_seq Seq.empty)
    (try List.combine f.inputs inputs
     with Invalid_argument _ ->
       [%log
         fatal
           "Mismatched number of arguments for call inputs,\n\
           \                      %d for %s and %d for call instruction"
           (List.length f.inputs)
           (f.nameo |> Option.value ~default:"noname")
           (List.length inputs)])

let step_call_internal (s : State.t) (p : Prog.t)
    ({
       target = { target = calln; attr_opt = attr };
       attr = { reserved_stack = copydepth; sp_diff };
       fallthrough;
     } :
      JCall.resolved_t) : (Store.t * Stack.elem_t, StopEvent.t) Result.t =
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
  let ndepth, regs, outputs =
    match attr with
    | Some { inputs; outputs } ->
        let regs = reg_build_from_input s.sto inputs f in
        (copydepth, regs, outputs)
    | None ->
        let regs = reg_build_from_input s.sto (build_inputs_from_sig f) f in
        (snd f.sp_boundary, regs, f.outputs)
  in
  let* nlocal =
    Store.build_local_frame s.sto p f.sp_boundary ndepth |> StopEvent.of_str_res
  in
  let regs =
    RegFile.add_reg regs
      { id = RegId.Register p.sp_num; offset = 0l; width = 8l }
      (Value.sp
         { func = calln; timestamp = TimeStamp.succ s.timestamp; offset = 0L })
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
  ( sto,
    {
      Stack.cursor = s.cursor;
      outputs;
      sregs = s.sto.regs;
      saved_sp;
      fallthrough;
    } )
  |> Result.ok

let step_ret (s : State.t) (p : Prog.t) ({ attr } : JRet.t)
    ({
       cursor = calln;
       outputs;
       sregs = regs';
       saved_sp = sp_saved;
       fallthrough = retn';
     } :
      Stack.elem_t) : (Store.t, StopEvent.t) Result.t =
  let values =
    List.fold_left
      (fun acc o ->
        let v = Store.eval_vn s.sto o |> Result.get_ok in
        v :: acc)
      [] attr
    |> List.rev
  in
  let output_values =
    try List.combine outputs values
    with Invalid_argument _ ->
      [%log fatal "Mismatched number of outputs for call outputs"]
  in
  Ok
    {
      s.sto with
      regs =
        RegFile.add_reg
          (List.fold_left
             (fun r (o, v) ->
               RegFile.add_reg r { id = o; offset = 0l; width = 8l } v)
             regs' output_values)
          { id = RegId.Register p.sp_num; offset = 0l; width = 8l }
          sp_saved;
    }

let step_JC = State.mk_step_JC step_call_internal
let step_JR = State.mk_step_JR step_ret

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) (curr : Cursor.t) :
    (Store.t, String.t) Result.t =
  match ins with
  | IA i -> Store.step_IA s i
  | ILS i -> Store.step_ILS s i
  | ISLS i -> Store.step_ISLS s i curr
  | IN i -> Store.step_IN s i

let step_jmp (p : Prog.t) (jmp : Jmp.t) (s : State.t) :
    (State.t, StopEvent.t) Result.t =
  match jmp with
  | JI ji -> State.step_JI s p ji |> StopEvent.of_str_res
  | JC jc -> step_JC s p jc
  | JR jr -> step_JR s p jr
  | JT _ -> Error (StopEvent.FailStop "unimplemented jump")

let step (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } ->
      step_jmp p jmp.jmp s |> Fun.flip StopEvent.add_loc jmp.loc
  | { remaining = i :: []; jmp } ->
      (let* sto' = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
       step_jmp p jmp.jmp { s with sto = sto' })
      |> Fun.flip StopEvent.add_loc i.loc
  | { remaining = i :: res; jmp } ->
      (let* sto' = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
       Ok { s with sto = sto'; cont = { remaining = res; jmp } })
      |> Fun.flip StopEvent.add_loc i.loc

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  if Loc.compare (Cont.get_loc s.cont) (0x404119L, 0) = 0 then (
    [%log info "interp: @,%a" State.pp s];
    let rdi =
      Store.get_reg s.sto { id = RegId.Register 56l; offset = 0l; width = 8l }
    in
    Store.load_string s.sto rdi |> Result.get_ok |> [%log info "rdi: %s"]);
  let s' = step p s in
  match s' with Error _ -> s' | Ok s' -> interp p s'
