open StdlibExt
open Basic
open Basic_collection

let ( let* ) = Result.bind

let eval_vn (vn : VarNode.t) (s : State.t) : Value.t =
  match vn with
  | Register r -> State.get_reg s r
  | Const v -> { value = v.value; width = v.width }

let eval_assignment (a : Assignable.t) (s : State.t) (outwidth : Int32.t) :
    (Value.t, String.t) Result.t =
  match a with
  | Avar vn -> Ok (eval_vn vn s)
  | Auop (u, vn) -> Common_language.NumericUop.eval u (eval_vn vn s) outwidth
  | Abop (b, lv, rv) ->
      Common_language.NumericBop.eval b (eval_vn lv s) (eval_vn rv s) outwidth

let step_ins (p : Prog.t) (ins : Inst.t) (s : State.t) :
    (State.t, String.t) Result.t =
  match ins with
  | Iassignment (a, o) ->
      let* v = eval_assignment a s o.width in
      Ok { s with regs = RegFile.add_reg s.regs o v; pc = Prog.fallthru p s.pc }
  | Iload (_, addrvn, outputid) ->
      let addr = eval_vn addrvn s in
      let v = State.load_mem s (Value.to_addr addr) outputid.width in
      Logger.debug "Loading %a from %a\n" Value.pp v Value.pp addr;
      Ok
        {
          s with
          regs = RegFile.add_reg s.regs outputid v;
          pc = Prog.fallthru p s.pc;
        }
  | Istore (_, addrvn, valuevn) ->
      let addr = eval_vn addrvn s in
      let v = eval_vn valuevn s in
      Logger.debug "Storing %a at %a\n" Value.pp v Value.pp addr;
      Ok
        {
          s with
          mem = Memory.store_mem s.mem (Value.to_addr addr) v;
          pc = Prog.fallthru p s.pc;
        }
  | Ijump l -> Ok { s with pc = l }
  | Icbranch (vn, l) ->
      let v = eval_vn vn s in
      if Value.isZero v then Ok { s with pc = Prog.fallthru p s.pc }
      else Ok { s with pc = l }
  | Ijump_ind vn ->
      let v = eval_vn vn s in
      Ok { s with pc = Value.to_loc v }
  | Inst.INop -> Ok { s with pc = Prog.fallthru p s.pc }
  | Inst.Iunimplemented -> Error "Unimplemented instruction"

let step (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  let* ins =
    Prog.get_ins p s.pc
    |> Option.to_result
         ~none:(Format.asprintf "No instruction at %a" Loc.pp s.pc)
  in
  step_ins p ins s

let rec interp (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  let s' = step p s in
  match s' with Error _ -> s' | Ok s' -> interp p s'
