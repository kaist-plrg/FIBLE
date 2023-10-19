open StdlibExt
open Basic
open Basic_collection

let ( let* ) = Result.bind

let eval_vn (vn : VarNode.t) (s : Store.t) : Value.t =
  match vn with
  | Register r -> Store.get_reg s r
  | Const v -> { value = v.value; width = v.width }

let eval_assignment (a : Assignable.t) (s : Store.t) (outwidth : Int32.t) :
    (Value.t, String.t) Result.t =
  match a with
  | Avar vn -> Ok (eval_vn vn s)
  | Auop (u, vn) -> Interp_Uop.eval_uop u (eval_vn vn s) outwidth
  | Abop (b, lv, rv) ->
      Interp_Bop.eval_bop b (eval_vn lv s) (eval_vn rv s) outwidth

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) :
    (Store.t, String.t) Result.t =
  match ins with
  | Iassignment (a, o) ->
      let* v = eval_assignment a s o.width in
      Ok { s with regs = RegFile.add_reg s.regs o v }
  | Iload (_, addrvn, outputid) ->
      let addr = eval_vn addrvn s in
      let v = Store.load_mem s (Value.to_addr addr) outputid.width in
      Format.printf "Loading %a from %a\n" Value.pp v Value.pp addr;
      Ok { s with regs = RegFile.add_reg s.regs outputid v }
  | Istore (_, addrvn, valuevn) ->
      let addr = eval_vn addrvn s in
      let v = eval_vn valuevn s in
      Format.printf "Storing %a at %a\n" Value.pp v Value.pp addr;
      Ok { s with mem = Memory.store_mem s.mem (Value.to_addr addr) v }
  | INop -> Ok s

let step_jmp (p : Prog.t) (jmp : Jmp.t_full) (s : State.t) :
    (State.t, String.t) Result.t =
  match jmp.jmp with
  | Jjump l ->
      let* ncont = Cont.of_block_loc p s.func l in
      Ok { s with cont = ncont }
  | Jfallthrough l ->
      let* ncont = Cont.of_block_loc p s.func l in
      Ok { s with cont = ncont }
  | Jjump_ind (vn, ls) ->
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
  | Jcall (calln, retn) ->
      let* ncont = Cont.of_func_entry_loc p calln in
      Ok
        { s with cont = ncont; stack = (s.func, retn) :: s.stack; func = calln }
  | Jcall_ind (callvn, retn) ->
      let calln = eval_vn callvn s.sto in
      let* ncont = Cont.of_func_entry_loc p (Value.to_loc calln) in
      Ok
        {
          s with
          cont = ncont;
          stack = (s.func, retn) :: s.stack;
          func = Value.to_loc calln;
        }
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
      Format.printf "%a%!\n" State.pp s';
      interp p s'
