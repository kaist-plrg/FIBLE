open StdlibExt
open Basic
open Basic_collection
open Common_language

let ( let* ) = Result.bind

let eval_vn (vn : VarNode.t) (s : State.t) : Value.t =
  match vn with
  | Register r -> RegFile.get_reg s.regs r
  | Const v -> Num { value = v.value; width = v.width }

let eval_assignment (a : Assignable.t) (s : State.t) (outwidth : Int32.t) :
    (Value.t, String.t) Result.t =
  match a with
  | Avar vn -> Ok (eval_vn vn s)
  | Auop (u, vn) -> Value.eval_uop u (eval_vn vn s) outwidth
  | Abop (b, lv, rv) -> Value.eval_bop b (eval_vn lv s) (eval_vn rv s) outwidth

let build_arg (s : State.t) (tagv : Common_language.Interop.tag) (v : Value.t) :
    Common_language.Interop.t =
  match tagv with
  | TString -> VString (Store.load_string s.sto v |> Result.get_ok)
  | T8 ->
      V8
        (Char.chr
           (match v with
           | Num { value; _ } -> Int64.to_int value
           | _ -> [%log fatal "Not a number"]))
  | T16 ->
      V16
        (Int64.to_int32
           (match v with
           | Num { value; _ } -> value
           | _ -> [%log fatal "Not a number"]))
  | T32 ->
      V32
        (Int64.to_int32
           (match v with
           | Num { value; _ } -> value
           | _ -> [%log fatal "Not a number"]))
  | T64 ->
      V64
        (match v with
        | Num { value; _ } ->
            if value < 0x1000L then value
            else
              Foreign.foreign "strdup"
                (Ctypes_static.( @-> ) Ctypes.string
                   (Ctypes.returning Ctypes_static.int64_t))
                "[null]"
        | _ ->
            Foreign.foreign "strdup"
              (Ctypes_static.( @-> ) Ctypes.string
                 (Ctypes.returning Ctypes_static.int64_t))
              "[null]")
  | _ -> [%log fatal "Not supported"]

let build_ret (s : State.t) (v : Common_language.Interop.t) : State.t =
  match v with
  | V8 c ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0L; width = 8l }
            (Value.Num { value = Int64.of_int (Char.code c); width = 8l });
      }
  | V16 i ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0L; width = 8l }
            (Value.Num { value = Int64.of_int32 i; width = 8l });
      }
  | V32 i ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0L; width = 8l }
            (Value.Num { value = Int64.of_int32 i; width = 8l });
      }
  | V64 i ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0L; width = 8l }
            (Value.Num { value = i; width = 8l });
      }
  | _ -> [%log fatal "Unsupported return type"]

let build_args (s : State.t) (fsig : Common_language.Interop.func_sig) :
    Common_language.Interop.t list =
  if List.length fsig.params > 6 then
    [%log fatal "At most 6 argument is supported for external functions"];
  let reg_list = [ 56L; 48L; 16L; 8L; 128L; 136L ] in
  let rec aux (acc : Common_language.Interop.t list)
      (param_tags : Common_language.Interop.tag list) (regs : Int64.t list) :
      Common_language.Interop.t list =
    match (param_tags, regs) with
    | [], _ -> List.rev acc
    | tag :: param_tags, reg :: regs ->
        let v = RegFile.get_reg s.regs { id = RegId.Register reg; width = 8l } in
        aux (build_arg s tag v :: acc) param_tags regs
    | _ -> [%log fatal "Not enough registers"]
  in
  aux [] fsig.params reg_list

let step_ins (p : Prog.t) (ins : Inst.t) (s : State.t) (func : Loc.t * Int64.t)
    : (State.t, String.t) Result.t =
  match ins with
  | Iassignment (a, o) ->
      let* v = eval_assignment a s o.width in
      Ok { s with regs = RegFile.add_reg s.regs o v }
  | Iload (_, addrvn, outputid) ->
      let addrv = eval_vn addrvn s in
      let* lv = Store.load_mem s.sto addrv outputid.width in
      [%log debug "Loading %a from %a" Value.pp lv Value.pp addrv];
      Ok { s with regs = RegFile.add_reg s.regs outputid lv }
  | Istore (_, addrvn, valuevn) ->
      let addrv = eval_vn addrvn s in
      let sv = eval_vn valuevn s in
      [%log debug "Storing %a at %a" Value.pp sv Value.pp addrv];
      let* sto' = Store.store_mem s.sto addrv sv in
      Ok { s with sto = sto' }
  | Ilload (cv, outputid) ->
      let addrv =
        Value.localP
          { func = fst func; timestamp = snd func; offset = cv.value }
      in
      let* lv = Store.load_mem s.sto addrv outputid.width in
      [%log debug "Loading %a from %a" Value.pp lv Value.pp addrv];
      Ok { s with regs = RegFile.add_reg s.regs outputid lv }
  | Ilstore (cv, valuevn) ->
      let addrv =
        Value.localP
          { func = fst func; timestamp = snd func; offset = cv.value }
      in
      let sv = eval_vn valuevn s in
      [%log debug "Storing %a at %a" Value.pp sv Value.pp addrv];
      let* sto' = Store.store_mem s.sto addrv sv in
      Ok { s with sto = sto' }
  | Ipload (cv, outputid) ->
      let addrv =
        Value.paramP
          { func = fst func; timestamp = snd func; offset = cv.value }
      in
      let* lv = Store.load_mem s.sto addrv outputid.width in
      [%log debug "Loading %a from %a" Value.pp lv Value.pp addrv];
      Ok { s with regs = RegFile.add_reg s.regs outputid lv }
  | Ipstore (cv, valuevn) ->
      let addrv =
        Value.paramP
          { func = fst func; timestamp = snd func; offset = cv.value }
      in
      let sv = eval_vn valuevn s in
      [%log debug "Storing %a at %a" Value.pp sv Value.pp addrv];
      let* sto' = Store.store_mem s.sto addrv sv in
      Ok { s with sto = sto' }
  | INop -> Ok s

let step_call (p : Prog.t) (spdiff : Int64.t) (outputs : RegId.t List.t)
    (inputs : VarNode.t List.t) (calln : Loc.t) (retn : Loc.t) (s : State.t) :
    (State.t, String.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr calln) p.externs with
  | None ->
      let* currf =
        Prog.get_func_opt p (fst s.func)
        |> Option.to_result
             ~none:
               (Format.asprintf "jcall: not found current function %a" Loc.pp
                  calln)
      in
      let* f =
        Prog.get_func_opt p calln
        |> Option.to_result
             ~none:(Format.asprintf "jcall: not found function %a" Loc.pp calln)
      in
      let* _ =
        if f.sp_diff = spdiff then Ok () else Error "jcall: spdiff not match"
      in
      let* ncont = Cont.of_func_entry_loc p calln in
      let sp_curr =
        RegFile.get_reg s.regs { id = RegId.Register 32L; width = 8l }
      in
      let* passing_val = Store.load_mem s.sto sp_curr 8l in
      let nlocal = Frame.store_mem Frame.empty 0L passing_val in
      let* sp_saved =
        Value.eval_bop Bop.Bint_add sp_curr
          (Num { value = spdiff; width = 8l })
          8l
      in
      Ok
        {
          State.timestamp = Int64Ext.succ s.timestamp;
          cont = ncont;
          stack = (s.func, currf.outputs, s.regs, sp_saved, retn) :: s.stack;
          regs =
            RegFile.add_reg
              (List.fold_left
                 (fun r i -> RegFile.add_reg r i (RegFile.get_reg s.regs i))
                 RegFile.empty
                 (currf.inputs
                 |> List.map (fun x -> { RegId.id = x; width = 8l })))
              { id = RegId.Register 32L; width = 8l }
              (Value.sp { func = calln; timestamp = Int64Ext.succ s.timestamp });
          func = (calln, Int64Ext.succ s.timestamp);
          sto =
            {
              s.sto with
              local =
                s.sto.local
                |> LocalMemory.add
                     (Local, calln, Int64Ext.succ s.timestamp)
                     nlocal
                |> LocalMemory.add
                     (Param, calln, Int64Ext.succ s.timestamp)
                     nlocal;
            };
        }
  | Some name ->
      [%log debug "Calling %s" name];
      let* fsig, _ =
        StringMap.find_opt name World.Environment.signature_map
        |> Option.to_result
             ~none:(Format.asprintf "No external function %s" name)
      in
      let args = build_args s fsig in
      let retv = World.Environment.request_call name args in

      let sp_curr =
        RegFile.get_reg s.regs { id = RegId.Register 32L; width = 8l }
      in
      let* sp_saved =
        Value.eval_bop Bop.Bint_add sp_curr
          (Num { value = spdiff; width = 8l })
          8l
      in

      let* ncont = Cont.of_block_loc p (fst s.func) retn in
      Ok
        (build_ret
           {
             s with
             regs =
               RegFile.add_reg s.regs
                 { id = RegId.Register 32L; width = 8l }
                 sp_saved;
             cont = ncont;
             stack = s.stack;
           }
           retv)

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
      let* loc = Value.try_loc (eval_vn vn s) in
      if LocSet.mem loc ls then
        let* ncont = Cont.of_block_loc p (fst s.func) loc in
        Ok { s with cont = ncont }
      else Error "jump_ind: Not a valid jump"
  | Jcbranch (vn, ift, iff) ->
      let v = eval_vn vn s in
      let* iz = Value.try_isZero v in
      if iz then
        let* ncont = Cont.of_block_loc p (fst s.func) iff in
        Ok { s with cont = ncont }
      else
        let* ncont = Cont.of_block_loc p (fst s.func) ift in
        Ok { s with cont = ncont }
  | Jcall (spdiff, outputs, inputs, calln, retn) ->
      step_call p spdiff outputs inputs calln retn s
  | Jcall_ind (spdiff, outputs, inputs, callvn, retn) ->
      let* calln = Value.try_loc (eval_vn callvn s) in
      step_call p spdiff outputs inputs calln retn s
  | Jret values -> (
      match s.stack with
      | [] -> Error (Format.asprintf "Empty stack")
      | (calln, outputs, regs', sp_saved, retn') :: stack' ->
          let* ncont = Cont.of_block_loc p (fst calln) retn' in
          let values =
            List.fold_left
              (fun acc o ->
                let v = eval_vn o s in
                v :: acc)
              [] values
            |> List.rev
          in
          let output_values = List.combine outputs values in
          Ok
            {
              State.cont = ncont;
              timestamp = s.timestamp;
              stack = stack';
              func = calln;
              sto = s.sto;
              regs =
                RegFile.add_reg
                  (List.fold_left
                     (fun r (o, v) ->
                       RegFile.add_reg r { id = o; width = 8l } v)
                     regs' output_values)
                  { id = RegId.Register 32L; width = 8l }
                  sp_saved;
            })
  | Junimplemented -> Error "unimplemented jump"

let step (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } -> step_jmp p jmp s
  | { remaining = i :: []; jmp } ->
      let* s' = step_ins p i.ins s s.func in
      step_jmp p jmp s'
  | { remaining = i :: res; jmp } ->
      let* s' = step_ins p i.ins s s.func in
      Ok { s' with cont = { remaining = res; jmp } }

let rec interp (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  let s' = step p s in
  match s' with
  | Error _ -> s'
  | Ok s' ->
      [%log debug "%a" State.pp s'];
      interp p s'
