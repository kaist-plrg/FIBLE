open Common
open Syn
open Sem

let build_inputs_from_sig (f : Func.t) : VarNode.t List.t =
  f.attr.inputs
  |> List.map (fun v -> VarNodeF.Register { id = v; offset = 0l; width = 8l })

let reg_build_from_values (sto : Store.t) (values : Value.t List.t) (f : Func.t)
    : (RegFile.t, String.t) Result.t =
  let* fv =
    try Ok (List.combine f.attr.inputs values)
    with Invalid_argument _ ->
      Error
        (Format.asprintf
           "Mismatched number of arguments for call inputs,\n\
           \                      %d for %s and %d for call instruction"
           (List.length f.attr.inputs)
           (f.nameo |> Option.value ~default:"noname")
           (List.length values))
  in
  List.fold_left
    (fun r (i, v) -> RegFile.add_reg r { id = i; offset = 0l; width = 8l } v)
    (RegFile.mapi (fun r _ -> Value.NonNum (Reg r)) sto.regs)
    fv
  |> Result.ok

let reg_build_from_input (sto : Store.t) (inputs : VarNode.t List.t)
    (f : Func.t) : (RegFile.t, String.t) Result.t =
  let* inputs = Store.eval_vn_list sto inputs in
  reg_build_from_values sto inputs f

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
    if f.attr.sp_diff = sp_diff then Ok ()
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
  let* ndepth, regs, outputs =
    match attr with
    | Some { inputs; outputs } ->
        let* regs =
          reg_build_from_values s.sto inputs f |> StopEvent.of_str_res
        in
        (copydepth, regs, outputs) |> Result.ok
    | None ->
        let* regs =
          reg_build_from_input s.sto (build_inputs_from_sig f) f
          |> StopEvent.of_str_res
        in
        (snd f.attr.sp_boundary, regs, f.attr.outputs) |> Result.ok
  in
  let* nlocal =
    Store.build_local_frame s.sto p f.attr.sp_boundary ndepth
    |> StopEvent.of_str_res
  in
  let regs =
    RegFile.add_reg regs
      { id = RegId.Register p.sp_num; offset = 0l; width = 8l }
      (Value.sp
         {
           func = calln;
           timestamp = TimeStamp.succ s.timestamp;
           multiplier = 1L;
           offset = 0L;
         })
  in
  let sto : Store.t =
    {
      regs;
      mem =
        s.sto.mem
        |> Memory.add_local_frame (calln, TimeStamp.succ s.timestamp) nlocal;
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

let step_ret (s : State.t) (p : Prog.t) ({ attr } : SRet.t)
    ({
       cursor = calln;
       outputs;
       sregs = regs';
       saved_sp = sp_saved;
       fallthrough = retn';
     } :
      Stack.elem_t) : (Store.t, StopEvent.t) Result.t =
  let values = attr in
  let* output_values =
    try Ok (List.combine outputs values)
    with Invalid_argument _ ->
      Error "Mismatched number of outputs for call outputs"
      |> StopEvent.of_str_res
  in
  let* merge_reg =
    List.fold_left
      (fun x (o, v) ->
        let* rf = x in
        match v with
        | Value.NonNum (Reg r) ->
            if RegId.compare o r <> 0 then Error "Unmatched reg"
            else rf |> Result.ok
        | _ ->
            RegFile.add_reg rf { id = o; offset = 0l; width = 8l } v
            |> Result.ok)
      (Ok regs') output_values
    |> StopEvent.of_str_res
  in
  Ok
    {
      s.sto with
      regs =
        RegFile.add_reg merge_reg
          { id = RegId.Register p.sp_num; offset = 0l; width = 8l }
          sp_saved;
    }

let action_JC = State.mk_action_JC step_call_internal
let action_JR = State.mk_action_JR step_ret

let step_ins (p : Prog.t) (s : Store.t) (curr : Cursor.t) (ins : Inst.t) :
    (StoreAction.t, String.t) Result.t =
  Inst.fold (Store.step_ILS s) (Store.step_ISLS s curr) (Store.step_IA s)
    (Store.step_IN s)
    (Store.step_SP World.Environment.x64_syscall_table s)
    ins

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
  | JT _ -> Error "unimplemented jump"

let step (p : Prog.t) (s : State.t) : (Action.t, StopEvent.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } -> step_jmp p jmp.jmp s |> StopEvent.of_str_res
  | { remaining = { ins = Fourth _; _ } :: []; jmp } ->
      step_jmp p jmp.jmp s |> StopEvent.of_str_res
  | { remaining = i :: []; jmp = { jmp = JI (JIntraF.Jfallthrough l) } } ->
      let* a = step_ins p s.sto s.cursor i.ins |> StopEvent.of_str_res in
      Action.of_store a (Some l) |> Result.ok
  | { remaining = i :: res; jmp } ->
      if List.is_empty res then
        StopEvent.FailStop "Not possible inst" |> Result.error
      else
        let* a = step_ins p s.sto s.cursor i.ins |> StopEvent.of_str_res in
        Action.of_store a None |> Result.ok

let action_store (p : Prog.t) (sto : Store.t) (a : StoreAction.t) :
    (Store.t, StopEvent.t) Result.t =
  match a with
  | Special _ -> Store.action_nop sto |> StopEvent.of_str_res
  | Assign (p, v) -> Store.action_assign sto p v |> StopEvent.of_str_res
  | Load (r, p, v) -> Store.action_load sto r p v |> StopEvent.of_str_res
  | Store (p, v) -> Store.action_store sto p v |> StopEvent.of_str_res
  | Nop -> Store.action_nop sto |> StopEvent.of_str_res

let action (p : Prog.t) (s : State.t) (a : Action.t) :
    (State.t, StopEvent.t) Result.t =
  match a with
  | StoreAction (a, lo) -> (
      let* sto = action_store p s.sto a in
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
  | ExternCall (name, sides, args, ft) -> (
      match World.Environment.request_call_opt name args with
      | Some retv -> State.action_extern p s sides retv ft
      | None -> StopEvent.NormalStop |> Result.error)
  | Call sc -> action_JC p s sc
  | TailCall st -> StopEvent.FailStop "unimplemented jump" |> Result.error
  | Ret sr -> action_JR p s sr

let action_with_computed_extern (p : Prog.t) (s : State.t) (a : Action.t)
    (sides : ('a * Bytes.t) List.t) (retv : Interop.t) :
    (State.t, StopEvent.t) Result.t =
  match a with
  | StoreAction (a, lo) -> (
      let* sto = action_store p s.sto a in
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
  | ExternCall (name, sidesn, args, ft) ->
      if List.length sides = List.length sidesn then
        let fake_sides =
          List.map
            (fun ((ptr, _), (_, bytes)) -> (ptr, bytes))
            (List.combine sidesn sides)
        in
        State.action_extern p s fake_sides retv ft
      else Error "Not same call target" |> StopEvent.of_str_res
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
