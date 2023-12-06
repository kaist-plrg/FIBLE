open StdlibExt
open Basic
open Basic_collection
open Common_language

let ( let* ) = Result.bind

let eval_vn (vn : VarNode.t) (s : Store.t) : Value.t =
  match vn with
  | Register r -> Store.get_reg s r
  | Const v -> Num { value = v.value; width = v.width }
  | Ram v ->
      Store.load_mem s (Num { value = v.value; width = v.width }) v.width
      |> Result.get_ok

let eval_assignment (a : Assignable.t) (s : Store.t) (outwidth : Int32.t) :
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
        sto =
          {
            s.sto with
            regs =
              RegFile.add_reg s.sto.regs
                { id = RegId.Register 0l; offset = 0l; width = 8l }
                (Value.Num { value = Int64.of_int (Char.code c); width = 8l });
          };
      }
  | V16 i ->
      {
        s with
        sto =
          {
            s.sto with
            regs =
              RegFile.add_reg s.sto.regs
                { id = RegId.Register 0l; offset = 0l; width = 8l }
                (Value.Num { value = Int64.of_int32 i; width = 8l });
          };
      }
  | V32 i ->
      {
        s with
        sto =
          {
            s.sto with
            regs =
              RegFile.add_reg s.sto.regs
                { id = RegId.Register 0l; offset = 0l; width = 8l }
                (Value.Num { value = Int64.of_int32 i; width = 8l });
          };
      }
  | V64 i ->
      {
        s with
        sto =
          {
            s.sto with
            regs =
              RegFile.add_reg s.sto.regs
                { id = RegId.Register 0l; offset = 0l; width = 8l }
                (Value.Num { value = i; width = 8l });
          };
      }
  | _ -> [%log fatal "Unsupported return type"]

let build_args (s : State.t) (fsig : Common_language.Interop.func_sig) :
    Common_language.Interop.t list =
  if List.length fsig.params > 6 then
    [%log fatal "At most 6 argument is supported for external functions"];
  let reg_list = [ 56l; 48l; 16l; 8l; 128l; 136l ] in
  let rec aux (acc : Common_language.Interop.t list)
      (param_tags : Common_language.Interop.tag list) (regs : Int32.t list) :
      Common_language.Interop.t list =
    match (param_tags, regs) with
    | [], _ -> List.rev acc
    | tag :: param_tags, reg :: regs ->
        let v =
          Store.get_reg s.sto
            { id = RegId.Register reg; offset = 0l; width = 8l }
        in
        aux (build_arg s tag v :: acc) param_tags regs
    | _ -> [%log fatal "Not enough registers"]
  in
  aux [] fsig.params reg_list

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) (func : Loc.t * Int64.t)
    : (Store.t, String.t) Result.t =
  match ins with
  | Iassignment (a, o) ->
      let* v = eval_assignment a s o.width in
      Ok { s with regs = RegFile.add_reg s.regs o v }
  | Iload (_, addrvn, outputid) ->
      let addrv = eval_vn addrvn s in
      let* lv = Store.load_mem s addrv outputid.width in
      [%log debug "Loading %a from %a" Value.pp lv Value.pp addrv];
      Ok { s with regs = RegFile.add_reg s.regs outputid lv }
  | Istore (_, addrvn, valuevn) ->
      let addrv = eval_vn addrvn s in
      let sv = eval_vn valuevn s in
      [%log debug "Storing %a at %a" Value.pp sv Value.pp addrv];
      Store.store_mem s addrv sv
  | Isload (cv, outputid) ->
      let addrv =
        Value.NonNum
          (SP { SPVal.func = fst func; timestamp = snd func; offset = cv.value })
      in
      let* lv = Store.load_mem s addrv outputid.width in
      [%log debug "Loading %a from %a" Value.pp lv Value.pp addrv];
      Ok { s with regs = RegFile.add_reg s.regs outputid lv }
  | Isstore (cv, valuevn) ->
      let addrv =
        Value.NonNum
          (SP { SPVal.func = fst func; timestamp = snd func; offset = cv.value })
      in
      let sv = eval_vn valuevn s in
      [%log debug "Storing %a at %a" Value.pp sv Value.pp addrv];
      Store.store_mem s addrv sv
  | INop -> Ok s

let step_call (p : Prog.t) (spdiff : Int64.t) (calln : Loc.t) (retn : Loc.t)
    (s : State.t) : (State.t, String.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr calln) p.externs with
  | None ->
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
        Store.get_reg s.sto { id = RegId.Register 32l; offset = 0l; width = 8l }
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
          stack = (s.func, sp_saved, retn) :: s.stack;
          func = (calln, Int64Ext.succ s.timestamp);
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
        }
  | Some name ->
      [%log debug "Calling %s" name];
      let* fsig, _ =
        StringMap.find_opt name World.Environment.signature_map
        |> Option.to_result
             ~none:(Format.asprintf "No external function %s" name)
      in
      let args = build_args s fsig in
      let _, retv = World.Environment.request_call name args in

      let sp_curr =
        Store.get_reg s.sto { id = RegId.Register 32l; offset = 0l; width = 8l }
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
             sto =
               {
                 s.sto with
                 regs =
                   RegFile.add_reg s.sto.regs
                     { id = RegId.Register 32l; offset = 0l; width = 8l }
                     sp_saved;
               };
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
  | Jcall (spdiff, calln, retn) -> step_call p spdiff calln retn s
  | Jcall_ind (spdiff, callvn, retn) ->
      let* calln = Value.try_loc (eval_vn callvn s.sto) in
      step_call p spdiff calln retn s
  | Jret retvn -> (
      let* retn = Value.try_loc (eval_vn retvn s.sto) in
      match s.stack with
      | [] -> Error (Format.asprintf "ret to %a: Empty stack" Loc.pp retn)
      | (calln, sp_saved, retn') :: stack' ->
          if Loc.compare retn retn' <> 0 then
            [%log
              debug "try to ret %a but supposed ret is %a" Loc.pp retn Loc.pp
                retn']
          else ();
          let* ncont = Cont.of_block_loc p (fst calln) retn' in
          Ok
            {
              s with
              cont = ncont;
              stack = stack';
              func = calln;
              sto =
                Store.add_reg s.sto
                  { id = RegId.Register 32l; offset = 0l; width = 8l }
                  sp_saved;
            })
  | Junimplemented -> Error "unimplemented jump"

let step (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } -> step_jmp p jmp s
  | { remaining = i :: []; jmp } ->
      let* sto' = step_ins p i.ins s.sto s.func in
      step_jmp p jmp { s with sto = sto' }
  | { remaining = i :: res; jmp } ->
      let* sto' = step_ins p i.ins s.sto s.func in
      Ok { s with sto = sto'; cont = { remaining = res; jmp } }

let rec interp (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  let s' = step p s in
  match s' with
  | Error _ -> s'
  | Ok s' ->
      [%log debug "%a" State.pp s'];
      interp p s'
