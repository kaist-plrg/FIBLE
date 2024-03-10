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
  | IA { expr; output } ->
      let* v = eval_assignment expr s output.width in
      Ok { s with regs = RegFile.add_reg s.regs output v }
  | ILS (Load { pointer; output; _ }) ->
      let addr = eval_vn pointer s in
      let v = Store.load_mem s (Value.to_addr addr) output.width in
      [%log debug "Loading %a from %a" Value.pp v Value.pp addr];
      Ok { s with regs = RegFile.add_reg s.regs output v }
  | ILS (Store { pointer; value; _ }) ->
      let addr = eval_vn pointer s in
      let v = eval_vn value s in
      [%log debug "Storing %a at %a" Value.pp v Value.pp addr];
      Ok { s with mem = Memory.store_mem s.mem (Value.to_addr addr) v }
  | IN _ -> Ok s

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

let step_tailcall (p : Prog.t) (calln : Loc.t) (s : State.t) :
    (State.t, String.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr calln) p.externs with
  | None ->
      let* ncont = Cont.of_func_entry_loc p calln in
      Ok { s with cont = ncont; stack = s.stack; func = calln }
  | Some name -> (
      [%log debug "Calling %s" name];
      let retpointer =
        Store.get_reg s.sto { id = RegId.Register 32l; offset = 0l; width = 8l }
      in
      let s_after =
        {
          s with
          sto =
            Store.add_reg s.sto
              { id = RegId.Register 32l; offset = 0l; width = 8l }
              (Value.of_int64 (Int64.add (Value.value_64 retpointer) 8L) 8l);
        }
      in
      match s_after.stack with
      | [] -> Error (Format.asprintf "ret to Empty stack")
      | (calln, retn') :: stack' ->
          let* ncont = Cont.of_block_loc p calln retn' in
          Ok { s with cont = ncont; stack = stack'; func = calln })

let step_ret (p : Prog.t) (retn : Value.t) (s : State.t) :
    (State.t, String.t) Result.t =
  match s.stack with
  | [] -> Error (Format.asprintf "ret to %a: Empty stack" Value.pp retn)
  | (calln, retn') :: stack' ->
      if Loc.compare (Value.to_loc retn) retn' = 0 then
        let* ncont = Cont.of_block_loc p calln (Value.to_loc retn) in
        Ok { s with cont = ncont; stack = stack'; func = calln }
      else
        Error
          (Format.asprintf "ret to %a: Expected %a" Value.pp retn Loc.pp retn')

let step_jmp (p : Prog.t) (jmp : Jmp.t_full) (s : State.t) :
    (State.t, String.t) Result.t =
  match jmp.jmp with
  | JI (Jjump l) ->
      let* ncont = Cont.of_block_loc p s.func l in
      Ok { s with cont = ncont }
  | JI (Jfallthrough l) ->
      let* ncont = Cont.of_block_loc p s.func l in
      Ok { s with cont = ncont }
  | JI (Jjump_ind { target; candidates; _ }) ->
      let v = eval_vn target s.sto in
      if LocSet.mem (Value.to_loc v) candidates then
        let* ncont = Cont.of_block_loc p s.func (Value.to_loc v) in
        Ok { s with cont = ncont }
      else Error "jump_ind: Not a valid jump"
  | JI (Jcbranch { condition; target_true; target_false }) ->
      let v = eval_vn condition s.sto in
      if Value.isZero v then
        let* ncont = Cont.of_block_loc p s.func target_false in
        Ok { s with cont = ncont }
      else
        let* ncont = Cont.of_block_loc p s.func target_true in
        Ok { s with cont = ncont }
  | JC { target = Cdirect { target; _ }; fallthrough } ->
      step_call p target fallthrough s
  | JC { target = Cind { target; _ }; fallthrough } ->
      let calln = eval_vn target s.sto in
      step_call p (Value.to_loc calln) fallthrough s
  | JT { target = Cdirect { target = calln; _ } } -> step_tailcall p calln s
  | JT { target = Cind { target = callvn; _ } } ->
      let calln = eval_vn callvn s.sto in
      step_tailcall p (Value.to_loc calln) s
  | JR { attr = retvn } ->
      let retn = eval_vn retvn s.sto in
      step_ret p retn s
  | JI Junimplemented -> Error "unimplemented jump"

let step (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } -> step_jmp p jmp s |> StopEvent.of_str_res
  | { remaining = i :: []; jmp } ->
      let* sto' = step_ins p i.ins s.sto |> StopEvent.of_str_res in
      step_jmp p jmp { s with sto = sto' } |> StopEvent.of_str_res
  | { remaining = i :: res; jmp } ->
      let* sto' = step_ins p i.ins s.sto |> StopEvent.of_str_res in
      Ok { s with sto = sto'; cont = { remaining = res; jmp } }
      |> StopEvent.of_str_res

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  let s' = step p s in
  match s' with
  | Error _ -> s'
  | Ok s' ->
      [%log debug "%a" State.pp s'];
      interp p s'
