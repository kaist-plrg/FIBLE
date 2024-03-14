open StdlibExt
open Notation
open Common
open Sem

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) (curr : Cursor.t) :
    (Store.t, String.t) Result.t =
  match ins with
  | IA i -> Store.step_IA s i
  | ILS i -> Store.step_ILS s i
  | ISLS i -> Store.step_ISLS s i curr
  | IN i -> Store.step_IN s i

let step_call (p : Prog.t) (copydepth : Int64.t) (spdiff : Int64.t)
    (calln : Loc.t) (retn : Loc.t) (s : State.t) :
    (State.t, StopEvent.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr calln) p.externs with
  | None ->
      (let* f =
         Prog.get_func_opt p calln
         |> Option.to_result
              ~none:
                (Format.asprintf "jcall: not found function %a" Loc.pp calln)
       in
       let* _ =
         if f.sp_diff = spdiff then Ok () else Error "jcall: spdiff not match"
       in
       let* ncont = Cont.of_func_entry_loc p calln in
       let sp_curr =
         Store.get_reg s.sto
           { id = RegId.Register 32l; offset = 0l; width = 8l }
       in
       let* passing_val = Store.load_mem s.sto sp_curr 8l in
       let* nlocal =
         Frame.store_mem
           (Frame.empty (fst f.sp_boundary) (snd f.sp_boundary))
           0L passing_val
       in
       let* sp_saved =
         Value.eval_bop Bop.Bint_add sp_curr
           (Num (NumericValue.of_int64 spdiff 8l))
           8l
       in
       Ok
         {
           State.timestamp = Int64Ext.succ s.timestamp;
           cont = ncont;
           stack = (s.cursor, sp_saved, retn) :: s.stack;
           cursor = { func = calln; tick = Int64Ext.succ s.timestamp };
           sto =
             {
               s.sto with
               regs =
                 RegFile.add_reg s.sto.regs
                   { id = RegId.Register 32l; offset = 0l; width = 8l }
                   (NonNum
                      (SP
                         {
                           SPVal.func = calln;
                           timestamp = Int64Ext.succ s.timestamp;
                           offset = 0L;
                         }));
               local =
                 LocalMemory.add
                   (calln, Int64Ext.succ s.timestamp)
                   nlocal s.sto.local;
             };
         })
      |> Result.map_error (fun e -> StopEvent.FailStop e)
  | Some name ->
      [%log debug "Calling %s" name];
      let* fsig, _ =
        StringMap.find_opt name World.Environment.signature_map
        |> Option.to_result
             ~none:
               (StopEvent.FailStop
                  (Format.asprintf "No external function %s" name))
      in
      let* bargs = Store.build_args s.sto fsig |> StopEvent.of_str_res in
      let values, args = bargs |> List.split in
      let* _, retv =
        match World.Environment.request_call name args with
        | EventReturn (_, retv) -> Ok ((), retv)
        | EventTerminate -> Error StopEvent.NormalStop
      in
      let sp_curr =
        Store.get_reg s.sto { id = RegId.Register 32l; offset = 0l; width = 8l }
      in
      let* sp_saved =
        Value.eval_bop Bop.Bint_add sp_curr
          (Num (NumericValue.of_int64 spdiff 8l))
          8l
        |> StopEvent.of_str_res
      in

      let* ncont =
        Cont.of_loc p (Cursor.get_func_loc s.cursor) retn
        |> StopEvent.of_str_res
      in

      let* sto =
        Store.build_ret
          (Store.add_reg s.sto
             { id = RegId.Register 32l; offset = 0l; width = 8l }
             sp_saved)
          retv
        |> StopEvent.of_str_res
      in
      Ok { s with sto; cont = ncont; stack = s.stack }

let step_jmp (p : Prog.t) (jmp : Jmp.t_full) (s : State.t) :
    (State.t, StopEvent.t) Result.t =
  match jmp.jmp with
  | JI j -> State.step_JI s p j |> StopEvent.of_str_res
  | JC
      {
        target = Cdirect { target; _ };
        fallthrough;
        attr = { reserved_stack; sp_diff };
      } ->
      step_call p reserved_stack sp_diff target fallthrough s
  | JC
      {
        target = Cind { target; _ };
        fallthrough;
        attr = { reserved_stack; sp_diff };
      } ->
      let* calln =
        Value.try_loc (Store.eval_vn s.sto target |> Result.get_ok)
        |> StopEvent.of_str_res
      in
      step_call p reserved_stack sp_diff calln fallthrough s
  | JR _ -> (
      match s.stack with
      | [] -> Error StopEvent.NormalStop
      | (calln, sp_saved, retn') :: stack' ->
          let* ncont =
            Cont.of_loc p (Cursor.get_func_loc calln) retn'
            |> StopEvent.of_str_res
          in
          Ok
            {
              s with
              cont = ncont;
              stack = stack';
              cursor = calln;
              sto =
                Store.add_reg s.sto
                  { id = RegId.Register 32l; offset = 0l; width = 8l }
                  sp_saved;
            })
  | JT _ -> Error (StopEvent.FailStop "unimplemented jump")

let step (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } -> step_jmp p jmp s
  | { remaining = i :: []; jmp } ->
      let* sto' = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
      step_jmp p jmp { s with sto = sto' }
  | { remaining = i :: res; jmp } ->
      let* sto' = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
      Ok { s with sto = sto'; cont = { remaining = res; jmp } }

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  let s' = step p s in
  match s' with Error _ -> s' | Ok s' -> interp p s'
