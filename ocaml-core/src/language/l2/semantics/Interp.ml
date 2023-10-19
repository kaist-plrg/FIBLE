open StdlibExt
open Basic
open Basic_collection
open Common_language

let ( let* ) = Result.bind

let eval_vn (vn : VarNode.t) (s : Store.t) : Value.t =
  match vn with
  | Register r -> Store.get_reg s r
  | Const v -> Num { value = v.value; width = v.width }

let eval_assignment (a : Assignable.t) (s : Store.t) (outwidth : Int32.t) :
    (Value.t, String.t) Result.t =
  match a with
  | Avar vn -> Ok (eval_vn vn s)
  | Auop (u, vn) -> Value.eval_uop u (eval_vn vn s) outwidth
  | Abop (b, lv, rv) -> Value.eval_bop b (eval_vn lv s) (eval_vn rv s) outwidth 

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) :
    (Store.t, String.t) Result.t =
  match ins with
  | Iassignment (a, o) ->
      let* v = eval_assignment a s o.width in
      Ok { s with regs = RegFile.add_reg s.regs o v }
  | Iload (_, addrvn, outputid) ->
      let addrv = eval_vn addrvn s in
      let* lv = Store.load_mem s addrv outputid.width in
      (* Format.printf "Loading %a from %a\n" Value.pp lv Value.pp addrv; *)
      Ok { s with regs = RegFile.add_reg s.regs outputid lv }
  | Istore (_, addrvn, valuevn) ->
      let addrv = eval_vn addrvn s in
      let sv = eval_vn valuevn s in
      (* Format.printf "Storing %a at %a\n" Value.pp sv Value.pp addrv; *)
      Store.store_mem s addrv sv
  | Isload (cv, otuputid) -> Error "unimplemented sload"
  | Isstore (cv, valuevn) -> Error "unimplemented sstore"
  | INop -> Ok s

let step_jmp (p : Prog.t) (jmp : Jmp.t_full) (s : State.t) :
    (State.t, String.t) Result.t =
  match jmp.jmp with
  | Jjump l ->
      let* ncont = Cont.of_block_loc p (fst s.func) l in
      Ok { s with cont = ncont }
  | Jfallthrough l ->
      let* ncont = Cont.of_block_loc p (fst s.func) l in
      Ok { s with cont = ncont }
  | Jjump_ind (vn, ls) ->
      let* loc = Value.try_loc (eval_vn vn s.sto) in
      if LocSet.mem loc ls then
        let* ncont = Cont.of_block_loc p (fst s.func) loc in
        Ok { s with cont = ncont }
      else Error "jump_ind: Not a valid jump"
  | Jcbranch (vn, ift, iff) ->
      let v = eval_vn vn s.sto in
      let* iz = Value.try_isZero v in
      if iz then
        let* ncont = Cont.of_block_loc p (fst s.func) iff in
        Ok { s with cont = ncont }
      else
        let* ncont = Cont.of_block_loc p (fst s.func) ift in
        Ok { s with cont = ncont }
  | Jcall (spdiff, calln, retn) ->
      let* f = Prog.get_func_opt p calln |> Option.to_result ~none:"jcall: not found function" in
      let* _ = if f.sp_diff = spdiff then Ok () else Error "jcall: spdiff not match" in
      let* ncont = Cont.of_func_entry_loc p calln in
      let sp_curr =
        Store.get_reg s.sto { id = RegId.Register 32L; width = 8l }
      in
      let* passing_val = Store.load_mem s.sto sp_curr 8l in
      let nlocal = Memory.store_mem Memory.empty 0L passing_val in
      let* sp_saved = Value.eval_bop Bop.Bint_add sp_curr (Num { value = spdiff; width = 8l }) 8l in
      Ok
        {
          State.timestamp = Int64Ext.succ s.timestamp;
          cont = ncont;
          stack = (s.func, sp_saved, retn) :: s.stack;
          func = (calln, Int64Ext.succ s.timestamp);
          sto =
            {
              s.sto with
              regs =
                RegFile.add_reg s.sto.regs
                  { id = RegId.Register 32L; width = 8l }
                  (SP
                     {
                       SPVal.func = calln;
                       timestamp = Int64Ext.succ s.timestamp;
                       offset = 0L;
                     });
              local =
                LocalMemory.add
                  (calln, Int64Ext.succ s.timestamp)
                  nlocal s.sto.local;
            };
        }
  | Jcall_ind (spdiff, callvn, retn) ->
      let* calln = Value.try_loc (eval_vn callvn s.sto) in
      let* f = Prog.get_func_opt p calln |> Option.to_result ~none:"jcall_ind: not found function" in
      let* _ = if f.sp_diff = spdiff then Ok () else Error "jcall_ind: spdiff not match" in
      let* ncont = Cont.of_func_entry_loc p calln in
      let sp_curr =
        Store.get_reg s.sto { id = RegId.Register 32L; width = 8l }
      in
      let* passing_val = Store.load_mem s.sto sp_curr 8l in
      let nlocal = Memory.store_mem Memory.empty 0L passing_val in
      let* sp_saved = Value.eval_bop Bop.Bint_add sp_curr (Num { value = spdiff; width = 8l }) 8l in
      Ok
        {
          State.timestamp = Int64Ext.succ s.timestamp;
          cont = ncont;
          stack = (s.func, sp_saved, retn) :: s.stack;
          func = (calln, Int64Ext.succ s.timestamp);
          sto =
            {
              s.sto with
              regs =
                RegFile.add_reg s.sto.regs
                  { id = RegId.Register 32L; width = 8l }
                  (SP
                     {
                       SPVal.func = calln;
                       timestamp = Int64Ext.succ s.timestamp;
                       offset = 0L;
                     });
              local =
                LocalMemory.add
                  (calln, Int64Ext.succ s.timestamp)
                  nlocal s.sto.local;
            };
        }
  | Jret retvn -> (
      let* retn = Value.try_loc (eval_vn retvn s.sto) in
      match s.stack with
      | [] -> Error (Format.asprintf "ret to %a: Empty stack" Loc.pp retn)
      | (calln, sp_saved, retn') :: stack' ->
          if Loc.compare retn retn' = 0 then
            let* ncont = Cont.of_block_loc p (fst calln) retn in
            Ok
              {
                s with
                cont = ncont;
                stack = stack';
                func = calln;
                sto =
                  Store.add_reg s.sto
                    { id = RegId.Register 32L; width = 8l }
                    sp_saved;
              }
          else
            Error
              (Format.asprintf "ret to %a: Expected %a" Loc.pp retn Loc.pp retn')
      )
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
