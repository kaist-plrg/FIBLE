open StdlibExt
open Notation
open Basic
open Basic_collection

let ( let* ) = Result.bind

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) :
    (Store.t, String.t) Result.t =
  match ins with
  | IA i -> Store.step_IA s i
  | ILS i -> Store.step_ILS s i
  | IN i -> Store.step_IN s i

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
  | JI (Jjump l) ->
      let* ncont = Cont.of_block_loc p s.func l in
      Ok { s with cont = ncont }
  | JI (Jfallthrough l) ->
      let* ncont = Cont.of_block_loc p s.func l in
      Ok { s with cont = ncont }
  | JI (Jjump_ind { target; candidates; _ }) ->
      let* v = Store.eval_vn s.sto target in
      if LocSet.mem (Value.to_loc v) candidates then
        let* ncont = Cont.of_block_loc p s.func (Value.to_loc v) in
        Ok { s with cont = ncont }
      else Error "jump_ind: Not a valid jump"
  | JI (Jcbranch { condition; target_true; target_false }) ->
      let* v = Store.eval_vn s.sto condition in
      if Value.isZero v then
        let* ncont = Cont.of_block_loc p s.func target_true in
        Ok { s with cont = ncont }
      else
        let* ncont = Cont.of_block_loc p s.func target_false in
        Ok { s with cont = ncont }
  | JC { target = Cdirect { target; _ }; fallthrough } ->
      step_call p target fallthrough s
  | JC { target = Cind { target; _ }; fallthrough } ->
      let* calln = Store.eval_vn s.sto target in
      step_call p (Value.to_loc calln) fallthrough s
  | JR { attr = retvn } -> (
      let* retn = Store.eval_vn s.sto retvn in
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
  | JT _ | JI Junimplemented -> Error "unimplemented jump"
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
