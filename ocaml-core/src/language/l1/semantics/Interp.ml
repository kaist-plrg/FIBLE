open StdlibExt
open Notation
open Basic
open Basic_collection

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
  | JI j -> State.step_JI s p j
  | JC { target = Cdirect { target; _ }; fallthrough } ->
      step_call p target fallthrough s
  | JC { target = Cind { target; _ }; fallthrough } ->
      let* calln = Store.eval_vn s.sto target in
      step_call p (Value.to_loc calln) fallthrough s
  | JT { target = Cdirect { target = calln; _ } } -> step_tailcall p calln s
  | JT { target = Cind { target = callvn; _ } } ->
      let* calln = Store.eval_vn s.sto callvn in
      step_tailcall p (Value.to_loc calln) s
  | JR { attr = retvn } ->
      let* retn = Store.eval_vn s.sto retvn in
      step_ret p retn s

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
  match s' with Error _ -> s' | Ok s' -> interp p s'
