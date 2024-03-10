open StdlibExt
open Basic
open Basic_collection
open Notation

let fallthrough (p : Prog.t) (pc : Loc.t) (s : (Store.t, String.t) Result.t) :
    (State.t, String.t) Result.t =
  s |> Result.map (fun sto : State.t -> { sto; pc = Prog.fallthru p pc })

let step_jump (p : Prog.t) (l : Loc.t) (s : State.t) :
    (State.t, String.t) Result.t =
  Ok { s with pc = l }

let step_cbranch (p : Prog.t) (condition : VarNode.t) (target : Loc.t)
    (s : State.t) : (State.t, String.t) Result.t =
  let* v = Store.eval_vn s.sto condition in
  if Value.isZero v then Ok { s with pc = Prog.fallthru p s.pc }
  else Ok { s with pc = target }

let step_jump_ind (p : Prog.t) (vn : VarNode.t) (s : State.t) :
    (State.t, String.t) Result.t =
  let* v = Store.eval_vn s.sto vn in
  Ok { s with pc = Value.to_loc v }

let step_ins (p : Prog.t) (ins : Inst.t) (s : State.t) :
    (State.t, String.t) Result.t =
  match ins with
  | IA i -> Store.step_IA s.sto i |> fallthrough p s.pc
  | ILS i -> Store.step_ILS s.sto i |> fallthrough p s.pc
  | IN i -> Store.step_IN s.sto i |> fallthrough p s.pc
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
      let* args = Store.build_args s.sto fsig |> StopEvent.of_str_res in
      match World.Environment.request_call name args with
      | World.Environment.EventTerminate -> Error StopEvent.NormalStop
      | World.Environment.EventReturn (_, retv) ->
          let* sto' =
            Store.build_ret
              (Store.add_reg s.sto
                 { id = RegId.Register 32l; offset = 0l; width = 8l }
                 (Value.of_int64 (Int64.add (Value.value_64 retpointer) 8L) 8l))
              retv
            |> StopEvent.of_str_res
          in
          { State.pc = Value.to_loc retaddr; sto = sto' } |> Result.ok)

let step (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  let* ins =
    Prog.get_ins p s.pc
    |> Option.to_result
         ~none:
           (StopEvent.FailStop
              (Format.asprintf "No instruction at %a" Loc.pp s.pc))
  in
  let* ns =
    step_ins p ins s |> Result.map_error (fun e -> StopEvent.FailStop e)
  in
  handle_extern p ns

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  let s' = step p s in
  match s' with Error _ -> s' | Ok s' -> interp p s'
