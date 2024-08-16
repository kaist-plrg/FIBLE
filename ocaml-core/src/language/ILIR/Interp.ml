open Common
open Syn
open Sem

let fallthrough (p : Prog.t) (pc : Loc.t) (s : (Store.t, String.t) Result.t) :
    (State.t, String.t) Result.t =
  s |> Result.map (fun sto : State.t -> { sto; pc = Prog.fallthru p pc })

let step_jump (p : Prog.t) (s : State.t) ({ target } : IJump.t) :
    (Action.t, String.t) Result.t =
  match Byte8Map.find_opt (Loc.get_addr target) p.externs with
  | None -> Ok (Action.jmp target)
  | Some name -> Ok (Action.externcall target)

let step_cbranch (p : Prog.t) (s : State.t)
    ({ condition; target } : Inst.ICbranch.t) : (Action.t, String.t) Result.t =
  let* v = Store.eval_vn s.sto condition in
  match Value.try_isZero v with
  | Ok true -> Ok (Action.jmp (Prog.fallthru p s.pc))
  | Ok false -> Ok (Action.jmp target)
  | Error e -> Error e

let step_jump_ind (p : Prog.t) (s : State.t) ({ target } : Inst.IJumpInd.t) :
    (Action.t, String.t) Result.t =
  let* l = Store.eval_vn s.sto target in
  let* l = Value.try_addr l in
  match Byte8Map.find_opt l p.externs with
  | None -> Ok (Action.jmp (Loc.of_addr l))
  | Some name -> Ok (Action.externcall (Loc.of_addr l))

let step_unimplemented (p : Prog.t) (s : State.t) (ins : Inst.IUnimplemented.t)
    : (Action.t, String.t) Result.t =
  Error "Unimplemented instruction"

let storeaction_to_action (p : Prog.t) (s : State.t) (a : Store.Action.t) :
    Action.t =
  Action.of_store a (Prog.fallthru p s.pc)

let step_ins (p : Prog.t) (s : State.t) (ins : Inst.t) :
    (Action.t, String.t) Result.t =
  Inst.fold
    (fun i -> Store.step_ILS s.sto i |> Result.map (storeaction_to_action p s))
    (fun i -> Store.step_IA s.sto i |> Result.map (storeaction_to_action p s))
    (fun i -> Store.step_IN s.sto i |> Result.map (storeaction_to_action p s))
    (fun i -> Store.step_SP s.sto i |> Result.map (storeaction_to_action p s))
    (step_cbranch p s) (step_jump p s) (step_jump_ind p s)
    (step_unimplemented p s) ins

let handle_extern (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  match Byte8Map.find_opt (Loc.get_addr s.pc) p.externs with
  | None -> Ok s
  | Some name -> (
      [%log debug "Calling %s" name];
      let retpointer =
        State.get_reg s { id = RegId.Register 32l; offset = 0l; width = 8l }
      in
      let* retaddr = State.load_mem s retpointer 8l |> StopEvent.of_str_res in
      let* fsig, _ =
        StringMap.find_opt name World.Environment.signature_map
        |> Option.to_result ~none:(StopEvent.FailStop "No external function")
      in
      let* sides, args = Store.build_args s.sto fsig |> StopEvent.of_str_res in
      match World.Environment.request_call name args with
      | World.Environment.EventTerminate -> Error StopEvent.NormalStop
      | World.Environment.EventReturn retv ->
          let* rv = Value.value_64 retpointer |> StopEvent.of_str_res in
          let* retaddr = Value.try_loc retaddr |> StopEvent.of_str_res in
          let* sto' =
            Store.build_ret
              (Store.add_reg s.sto
                 { id = RegId.Register 32l; offset = 0l; width = 8l }
                 (Value.of_int64 (Int64.add rv 8L) 8l))
              retv
            |> StopEvent.of_str_res
          in
          { State.pc = retaddr; sto = sto' } |> Result.ok)

let action_store (p : Prog.t) (sto : Store.t) (a : StoreAction.t) :
    (Store.t, StopEvent.t) Result.t =
  match a with
  | Special v -> Store.action_nop sto |> StopEvent.of_str_res
  | Assign (p, v) -> Store.action_assign sto p v |> StopEvent.of_str_res
  | Load (r, p, v) -> Store.action_load sto r p v |> StopEvent.of_str_res
  | Store (p, v) -> Store.action_store sto p v |> StopEvent.of_str_res
  | Nop -> Store.action_nop sto |> StopEvent.of_str_res

let action (p : Prog.t) (s : State.t) (a : Action.t) :
    (State.t, StopEvent.t) Result.t =
  match a with
  | StoreAction (a, l) ->
      let* sto = action_store p s.sto a in
      { State.sto; pc = l } |> Result.ok
  | Jmp l -> { s with pc = l } |> Result.ok
  | ExternCall l -> handle_extern p s

let step (p : Prog.t) (s : State.t) : (Action.t, StopEvent.t) Result.t =
  let* ins =
    Prog.get_ins p s.pc
    |> Option.to_result
         ~none:
           (StopEvent.FailStop
              (Format.asprintf "No instruction at %a" Loc.pp s.pc))
  in
  step_ins p s ins |> Result.map_error (fun e -> StopEvent.FailStop e)

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  let* a = step p s |> Fun.flip StopEvent.add_loc s.pc in
  let* s' = action p s a |> Fun.flip StopEvent.add_loc s.pc in
  interp p s'
