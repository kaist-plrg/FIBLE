open StdlibExt
open Basic
open Basic_collection

let ( let* ) = Result.bind

let eval_vn (vn : VarNode.t) (s : Store.t) : Value.t =
  match vn with
  | Register r -> Store.get_reg s r
  | Const v -> Value.of_int64 v.value v.width
  | Ram v -> Store.load_mem s v.value v.width

let eval_assignment (a : Assignable.t) (s : Store.t) (outwidth : Int32.t) :
    (Value.t, String.t) Result.t =
  match a with
  | Avar vn -> Ok (eval_vn vn s)
  | Auop (u, vn) -> Common_language.NumericUop.eval u (eval_vn vn s) outwidth
  | Abop (b, lv, rv) ->
      Common_language.NumericBop.eval b (eval_vn lv s) (eval_vn rv s) outwidth

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) :
    (Store.t, String.t) Result.t =
  match ins with
  | Iassignment (a, o) ->
      let* v = eval_assignment a s o.width in
      Ok { s with regs = RegFile.add_reg s.regs o v }
  | Iload (_, addrvn, outputid) ->
      let addr = eval_vn addrvn s in
      let v = Store.load_mem s (Value.to_addr addr) outputid.width in
      [%log debug "Loading %a from %a" Value.pp v Value.pp addr];
      Ok { s with regs = RegFile.add_reg s.regs outputid v }
  | Istore (_, addrvn, valuevn) ->
      let addr = eval_vn addrvn s in
      let v = eval_vn valuevn s in
      [%log debug "Storing %a at %a" Value.pp v Value.pp addr];
      Ok { s with mem = Memory.store_mem s.mem (Value.to_addr addr) v }
  | INop -> Ok s

let step_call (p : Prog.t) (calln : Loc.t) (retn : Loc.t) (s : State.t) :
    (State.t, String.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr calln) p.externs with
  | None ->
      let* ncont = Cont.of_func_entry_loc p calln in
      Ok
        { s with cont = ncont; stack = (s.func, retn) :: s.stack; func = calln }
  | Some name ->
      [%log debug "Calling %s" name];
      let retpointer =
        Store.get_reg s.sto { id = RegId.Register 32l; offset = 0l; width = 8l }
      in
      let* ncont = Cont.of_block_loc p s.func retn in
      Ok
        {
          s with
          sto =
            Store.add_reg s.sto
              { id = RegId.Register 32l; offset = 0l; width = 8l }
              (Value.of_int64 (Int64.add (Value.value_64 retpointer) 8L) 8l);
          cont = ncont;
          stack = s.stack;
        }

let step_jmp (p : Prog.t) (jmp : Jmp.t_full) (s : State.t) :
    (State.t, String.t) Result.t =
  match jmp.jmp with
  | Jjump l ->
      let* ncont = Cont.of_block_loc p s.func l in
      Ok { s with cont = ncont }
  | Jfallthrough l ->
      let* ncont = Cont.of_block_loc p s.func l in
      Ok { s with cont = ncont }
  | Jjump_ind (vn, ls, _) ->
      let v = eval_vn vn s.sto in
      if LocSet.mem (Value.to_loc v) ls then
        let* ncont = Cont.of_block_loc p s.func (Value.to_loc v) in
        Ok { s with cont = ncont }
      else Error "jump_ind: Not a valid jump"
  | Jcbranch (vn, ift, iff) ->
      let v = eval_vn vn s.sto in
      if Value.isZero v then
        let* ncont = Cont.of_block_loc p s.func iff in
        Ok { s with cont = ncont }
      else
        let* ncont = Cont.of_block_loc p s.func ift in
        Ok { s with cont = ncont }
  | Jcall (calln, retn) -> step_call p calln retn s
  | Jcall_ind (callvn, retn) ->
      let calln = eval_vn callvn s.sto in
      step_call p (Value.to_loc calln) retn s
  | Jret retvn -> (
      let retn = eval_vn retvn s.sto in
      match s.stack with
      | [] -> Error (Format.asprintf "ret to %a: Empty stack" Value.pp retn)
      | (calln, retn') :: stack' ->
          if Loc.compare (Value.to_loc retn) retn' = 0 then
            let* ncont = Cont.of_block_loc p calln (Value.to_loc retn) in
            Ok { s with cont = ncont; stack = stack'; func = calln }
          else
            Error
              (Format.asprintf "ret to %a: Expected %a" Value.pp retn Loc.pp
                 retn'))
  | Junimplemented -> Error "unimplemented jump"
  | JswitchStop _ -> Error "switch stop"

let step (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } -> step_jmp p jmp s
  | { remaining = i :: []; jmp } ->
      let* sto' = step_ins p i.ins s.sto in
      step_jmp p jmp { s with sto = sto' }
  | { remaining = i :: res; jmp } ->
      let* sto' = step_ins p i.ins s.sto in
      Ok { s with sto = sto'; cont = { remaining = res; jmp } }

let rec interp (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  let s' = step p s in
  match s' with
  | Error _ -> s'
  | Ok s' ->
      [%log debug "%a" State.pp s'];
      interp p s'
