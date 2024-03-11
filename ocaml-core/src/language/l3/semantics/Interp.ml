open StdlibExt
open Basic
open Basic_collection
open Common_language
open Notation

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) (curr : Loc.t * Int64.t)
    : (Store.t, String.t) Result.t =
  match ins with
  | IA i -> Store.step_IA s i
  | ILS i -> Store.step_ILS s i
  | ISLS i -> Store.step_ISLS s i curr
  | IN i -> Store.step_IN s i

let get_func_from (p : Prog.t) (target : Loc.t) : (Func.t, String.t) Result.t =
  Prog.get_func_opt p target
  |> Option.to_result
       ~none:(Format.asprintf "jcall: not found function %a" Loc.pp target)

let get_current_function (s : State.t) (p : Prog.t) :
    (Func.t, String.t) Result.t =
  get_func_from p (fst s.func)

let get_sp_curr (s : State.t) (p : Prog.t) : Value.t =
  Store.get_reg s.sto { id = RegId.Register p.sp_num; offset = 0l; width = 8l }

let build_local_frame (s : State.t) (p : Prog.t) (bnd : Int64.t * Int64.t)
    (copydepth : Int64.t) =
  let sp_curr = get_sp_curr s p in
  let* passing_vals =
    List.fold_left
      (fun acc (i, x) ->
        match acc with
        | Error _ -> acc
        | Ok acc ->
            let* addr = x in
            let* v = Store.load_mem s.sto addr 8l in
            Ok ((i, v) :: acc))
      (Ok [])
      (Int64.div copydepth 8L |> Int64.succ |> Int64.to_int
      |> Fun.flip List.init (fun x ->
             ( Int64.of_int (x * 8),
               Value.eval_bop Bop.Bint_add sp_curr
                 (Num (NumericValue.of_int64 (Int64.of_int (x * 8)) 8l))
                 8l )))
  in
  List.fold_left
    (fun acc (i, j) -> Result.bind acc (fun acc -> Frame.store_mem acc i j))
    (Frame.empty (fst bnd) (snd bnd) |> Result.ok)
    passing_vals

let build_saved_sp (s : State.t) (p : Prog.t) (spdiff : Int64.t) :
    (Value.t, String.t) Result.t =
  let sp_curr = get_sp_curr s p in
  Value.eval_bop Bop.Bint_add sp_curr (Num (NumericValue.of_int64 spdiff 8l)) 8l

let step_call_extern (p : Prog.t) (spdiff : Int64.t) (name : String.t)
    (retn : Loc.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  [%log debug "Calling %s" name];
  let* fsig, _ =
    StringMap.find_opt name World.Environment.signature_map
    |> Option.to_result
         ~none:
           (StopEvent.FailStop (Format.asprintf "No external function %s" name))
  in
  let* bargs = Store.build_args s.sto fsig |> StopEvent.of_str_res in
  let values, args = bargs |> List.split in
  [%log
    debug "Call values: %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Value.pp)
      values];
  [%log
    debug "Call args: %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Interop.pp)
      args];
  let* sides, retv =
    match World.Environment.request_call name args with
    | EventReturn (sides, retv) -> Ok (sides, retv)
    | EventTerminate -> Error StopEvent.NormalStop
  in
  [%log
    debug "Side values: %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun fmt (i, v) ->
           Format.fprintf fmt "%d: %a" i Interop.pp v))
      sides];
  let* sp_saved = build_saved_sp s p spdiff |> StopEvent.of_str_res in
  let* ncont = Cont.of_block_loc p (fst s.func) retn |> StopEvent.of_str_res in
  let* sto_side =
    Store.build_sides s.sto values sides |> StopEvent.of_str_res
  in
  let* sto =
    Store.build_ret
      (Store.add_reg sto_side
         { id = RegId.Register p.sp_num; offset = 0l; width = 8l }
         sp_saved)
      retv
    |> StopEvent.of_str_res
  in
  Ok { s with sto; cont = ncont; stack = s.stack }

let step_call (p : Prog.t) (copydepth : Int64.t) (spdiff : Int64.t)
    (outputs : RegId.t List.t) (inputs : VarNode.t List.t) (calln : Loc.t)
    (retn : Loc.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr calln) p.externs with
  | None ->
      let* currf = get_current_function s p |> StopEvent.of_str_res in
      let* f = get_func_from p calln |> StopEvent.of_str_res in
      let* _ =
        if f.sp_diff = spdiff then Ok ()
        else Error (StopEvent.FailStop "jcall: spdiff not match")
      in
      let* ncont = Cont.of_func_entry_loc p calln |> StopEvent.of_str_res in
      let* nlocal =
        build_local_frame s p f.sp_boundary copydepth |> StopEvent.of_str_res
      in
      let* sp_saved = build_saved_sp s p spdiff |> StopEvent.of_str_res in
      Ok
        {
          State.timestamp = Int64Ext.succ s.timestamp;
          cont = ncont;
          stack = (s.func, outputs, s.sto.regs, sp_saved, retn) :: s.stack;
          func = (calln, Int64Ext.succ s.timestamp);
          sto =
            {
              s.sto with
              regs =
                RegFile.add_reg
                  (List.fold_left
                     (fun r (i, v) ->
                       RegFile.add_reg r
                         { id = i; offset = 0l; width = 8l }
                         (Store.eval_vn s.sto v |> Result.get_ok))
                     (RegFile.of_seq Seq.empty)
                     (try List.combine f.inputs inputs
                      with Invalid_argument _ ->
                        [%log
                          fatal
                            "Mismatched number of arguments for call inputs,\n\
                            \                      %d for %s and %d for call \
                             instruction"
                            (List.length f.inputs)
                            (f.nameo |> Option.value ~default:"noname")
                            (List.length inputs)]))
                  { id = RegId.Register 32l; offset = 0l; width = 8l }
                  (Value.sp
                     {
                       func = calln;
                       timestamp = Int64Ext.succ s.timestamp;
                       offset = 0L;
                     });
              local =
                s.sto.local
                |> LocalMemory.add (calln, Int64Ext.succ s.timestamp) nlocal;
            };
        }
  | Some name -> step_call_extern p spdiff name retn s

let step_call_ind (p : Prog.t) (copydepth : Int64.t) (spdiff : Int64.t)
    (calln : Loc.t) (retn : Loc.t) (s : State.t) :
    (State.t, StopEvent.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr calln) p.externs with
  | None ->
      let* currf = get_current_function s p |> StopEvent.of_str_res in
      let* f = get_func_from p calln |> StopEvent.of_str_res in
      let* _ =
        if f.sp_diff = spdiff then Ok ()
        else Error (StopEvent.FailStop "jcall_ind: spdiff not match")
      in
      (* TODO: think ind copydepth
         let* _ =
           if snd f.sp_boundary <= copydepth then Ok ()
           else Error "jcall_ind: copydepth not match"
         in
      *)
      let* ncont = Cont.of_func_entry_loc p calln |> StopEvent.of_str_res in
      let* nlocal =
        build_local_frame s p f.sp_boundary (snd f.sp_boundary)
        |> StopEvent.of_str_res
      in
      let* sp_saved = build_saved_sp s p spdiff |> StopEvent.of_str_res in
      Ok
        {
          State.timestamp = Int64Ext.succ s.timestamp;
          cont = ncont;
          stack = (s.func, f.outputs, s.sto.regs, sp_saved, retn) :: s.stack;
          func = (calln, Int64Ext.succ s.timestamp);
          sto =
            {
              s.sto with
              regs =
                RegFile.add_reg
                  (List.fold_left
                     (fun r i ->
                       RegFile.add_reg r
                         { id = i; offset = 0l; width = 8l }
                         (Store.get_reg s.sto
                            { id = i; offset = 0l; width = 8l }))
                     (RegFile.of_seq Seq.empty) f.inputs)
                  { id = RegId.Register 32l; offset = 0l; width = 8l }
                  (Value.sp
                     {
                       func = calln;
                       timestamp = Int64Ext.succ s.timestamp;
                       offset = 0L;
                     });
              local =
                s.sto.local
                |> LocalMemory.add (calln, Int64Ext.succ s.timestamp) nlocal;
            };
        }
  | Some name -> step_call_extern p spdiff name retn s

let step_JC (s : State.t) (p : Prog.t) (jc : JCall.t) :
    (State.t, StopEvent.t) Result.t =
  match jc with
  | {
   target = Cdirect { attr = { outputs; inputs }; target };
   attr = { reserved_stack; sp_diff };
   fallthrough;
  } ->
      step_call p reserved_stack sp_diff outputs inputs target fallthrough s
  | {
   target = Cind { target };
   attr = { reserved_stack; sp_diff };
   fallthrough;
  } ->
      let* calln =
        Result.bind (Store.eval_vn s.sto target) Value.try_loc
        |> StopEvent.of_str_res
      in
      step_call_ind p reserved_stack sp_diff calln fallthrough s

let step_JR (s : State.t) (p : Prog.t) ({ attr } : JRet.t) :
    (State.t, StopEvent.t) Result.t =
  match s.stack with
  | [] -> Error StopEvent.NormalStop
  | (calln, outputs, regs', sp_saved, retn') :: stack' ->
      let* ncont =
        Cont.of_block_loc p (fst calln) retn' |> StopEvent.of_str_res
      in
      let values =
        List.fold_left
          (fun acc o ->
            let v = Store.eval_vn s.sto o |> Result.get_ok in
            v :: acc)
          [] attr
        |> List.rev
      in
      let output_values =
        try List.combine outputs values
        with Invalid_argument _ ->
          [%log fatal "Mismatched number of outputs for call outputs"]
      in
      Ok
        {
          State.cont = ncont;
          timestamp = s.timestamp;
          stack = stack';
          func = calln;
          sto =
            {
              s.sto with
              regs =
                RegFile.add_reg
                  (List.fold_left
                     (fun r (o, v) ->
                       RegFile.add_reg r { id = o; offset = 0l; width = 8l } v)
                     regs' output_values)
                  { id = RegId.Register 32l; offset = 0l; width = 8l }
                  sp_saved;
            };
        }

let step_jmp (p : Prog.t) (jmp : Jmp.t_full) (s : State.t) :
    (State.t, StopEvent.t) Result.t =
  match jmp.jmp with
  | JI ji -> State.step_JI s p ji |> StopEvent.of_str_res
  | JC jc -> step_JC s p jc
  | JR jr -> step_JR s p jr
  | JT _ -> Error (StopEvent.FailStop "unimplemented jump")

let step (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } -> step_jmp p jmp s
  | { remaining = i :: []; jmp } ->
      let* sto' =
        step_ins p i.ins s.sto s.func
        |> Result.map_error (fun e -> Format.asprintf "%a: %s" Loc.pp i.loc e)
        |> StopEvent.of_str_res
      in
      step_jmp p jmp { s with sto = sto' }
  | { remaining = i :: res; jmp } ->
      let* sto' =
        step_ins p i.ins s.sto s.func
        |> Result.map_error (fun e -> Format.asprintf "%a: %s" Loc.pp i.loc e)
        |> StopEvent.of_str_res
      in
      Ok { s with sto = sto'; cont = { remaining = res; jmp } }

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  let s' = step p s in
  match s' with Error _ -> s' | Ok s' -> interp p s'
