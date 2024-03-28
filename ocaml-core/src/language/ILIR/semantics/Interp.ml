open StdlibExt
open Common
open Notation
open Sem

let fallthrough (p : Prog.t) (pc : Loc.t) (s : (Store.t, String.t) Result.t) :
    (State.t, String.t) Result.t =
  s |> Result.map (fun sto : State.t -> { sto; pc = Prog.fallthru p pc })

let step_jump (p : Prog.t) (l : Loc.t) (s : State.t) :
    (Action.t, String.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr l) p.externs with
  | None -> Ok (Action.jmp l)
  | Some name -> Ok (Action.externcall l)

let step_cbranch (p : Prog.t) (condition : VarNode.t) (target : Loc.t)
    (s : State.t) : (Action.t, String.t) Result.t =
  let* v = Store.eval_vn s.sto condition in
  match Value.try_isZero v with
  | Ok true -> Ok (Action.jmp (Prog.fallthru p s.pc))
  | Ok false -> Ok (Action.jmp target)
  | Error e -> Error e

let step_jump_ind (p : Prog.t) (vn : VarNode.t) (s : State.t) :
    (Action.t, String.t) Result.t =
  let* l = Store.eval_vn s.sto vn in
  let* l = Value.try_addr l in
  match AddrMap.find_opt l p.externs with
  | None -> Ok (Action.jmp (Loc.of_addr l))
  | Some name -> Ok (Action.externcall (Loc.of_addr l))

let step_ins (p : Prog.t) (ins : Inst.t) (s : State.t) :
    (Action.t, String.t) Result.t =
  match ins with
  | IA i ->
      let* a = Store.step_IA s.sto i in
      Action.of_store a (Prog.fallthru p s.pc) |> Result.ok
  | ILS i ->
      let* a = Store.step_ILS s.sto i in
      Action.of_store a (Prog.fallthru p s.pc) |> Result.ok
  | IN i ->
      let* a = Store.step_IN s.sto i in
      Action.of_store a (Prog.fallthru p s.pc) |> Result.ok
  | Ijump l -> step_jump p l s
  | Icbranch { condition; target } -> step_cbranch p condition target s
  | Ijump_ind vn -> step_jump_ind p vn s
  | Iunimplemented -> Error "Unimplemented instruction"

let handle_extern (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr s.pc) p.externs with
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
      let* bargs = Store.build_args s.sto fsig |> StopEvent.of_str_res in
      let values, args = bargs |> List.split in
      match World.Environment.request_call name args with
      | World.Environment.EventTerminate -> Error StopEvent.NormalStop
      | World.Environment.EventReturn (_, retv) ->
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

let action (p : Prog.t) (s : State.t) (a : Action.t) :
    (State.t, StopEvent.t) Result.t =
  match a with
  | StoreAction (a, l) ->
      let* sto = Store.action s.sto a |> StopEvent.of_str_res in
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
  step_ins p ins s |> Result.map_error (fun e -> StopEvent.FailStop e)

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  let* a = step p s |> Fun.flip StopEvent.add_loc s.pc in
  let* s' = action p s a |> Fun.flip StopEvent.add_loc s.pc in
  interp p s'
