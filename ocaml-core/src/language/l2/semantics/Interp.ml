open StdlibExt
open Basic
open Basic_collection
open Common_language

let ( let* ) = Result.bind

let eval_vn (vn : VarNode.t) (s : Store.t) : Value.t =
  match vn with
  | Register r -> Store.get_reg s r
  | Const v -> Num (NumericValue.of_int64 v.value v.width)
  | Ram v ->
      Store.load_mem s (Num (NumericValue.of_int64 v.value 8l)) v.width
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
           | Num value -> Int64.to_int (NumericValue.value_64 value)
           | _ -> [%log fatal "Not a number"]))
  | T16 ->
      V16
        (Int64.to_int32
           (match v with
           | Num value -> NumericValue.value_64 value
           | _ -> [%log fatal "Not a number"]))
  | T32 ->
      V32
        (Int64.to_int32
           (match v with
           | Num value -> NumericValue.value_64 value
           | _ -> [%log fatal "Not a number"]))
  | T64 ->
      V64
        (match v with
        | Num value ->
            if NumericValue.value_64 value < 0x1000L then
              NumericValue.value_64 value
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
                (Value.Num
                   (NumericValue.of_int64 (Int64.of_int (Char.code c)) 8l));
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
                (Value.Num (NumericValue.of_int64 (Int64.of_int32 i) 8l));
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
                (Value.Num (NumericValue.of_int64 (Int64.of_int32 i) 8l));
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
                (Value.Num (NumericValue.of_int64 i 8l));
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
  | Iassignment { expr; output } ->
      let* v = eval_assignment expr s output.width in
      Ok { s with regs = RegFile.add_reg s.regs output v }
  | Iload { pointer; output; _ } ->
      let addrv = eval_vn pointer s in
      let* lv = Store.load_mem s addrv output.width in
      [%log debug "Loading %a from %a" Value.pp lv Value.pp addrv];
      Ok { s with regs = RegFile.add_reg s.regs output lv }
  | Istore { pointer; value; _ } ->
      let addrv = eval_vn pointer s in
      let sv = eval_vn value s in
      [%log debug "Storing %a at %a" Value.pp sv Value.pp addrv];
      Store.store_mem s addrv sv
  | Isload { offset; output } ->
      let addrv =
        Value.NonNum
          (SP
             {
               SPVal.func = fst func;
               timestamp = snd func;
               offset = offset.value;
             })
      in
      let* lv = Store.load_mem s addrv output.width in
      [%log debug "Loading %a from %a" Value.pp lv Value.pp addrv];
      Ok { s with regs = RegFile.add_reg s.regs output lv }
  | Isstore { offset; value } ->
      let addrv =
        Value.NonNum
          (SP
             {
               SPVal.func = fst func;
               timestamp = snd func;
               offset = offset.value;
             })
      in
      let sv = eval_vn value s in
      [%log debug "Storing %a at %a" Value.pp sv Value.pp addrv];
      Store.store_mem s addrv sv
  | INop -> Ok s

let step_call (p : Prog.t) (copydepth : Int64.t) (spdiff : Int64.t)
    (calln : Loc.t) (retn : Loc.t) (s : State.t) : (State.t, String.t) Result.t
    =
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
          (Num (NumericValue.of_int64 spdiff 8l))
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
          (Num (NumericValue.of_int64 spdiff 8l))
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
  | Jjump_ind { target; candidates; _ } ->
      let* loc = Value.try_loc (eval_vn target s.sto) in
      if LocSet.mem loc candidates then
        let* ncont = Cont.of_block_loc p (fst s.func) loc in
        Ok { s with cont = ncont }
      else Error "jump_ind: Not a valid jump"
  | Jcbranch { condition; target_true; target_false } ->
      let v = eval_vn condition s.sto in
      let* iz = Value.try_isZero v in
      if iz then
        let* ncont = Cont.of_block_loc p (fst s.func) target_false in
        Ok { s with cont = ncont }
      else
        let* ncont = Cont.of_block_loc p (fst s.func) target_true in
        Ok { s with cont = ncont }
  | Jcall { reserved_stack; sp_diff; target; fallthrough } ->
      step_call p reserved_stack sp_diff target fallthrough s
  | Jcall_ind { reserved_stack; sp_diff; target; fallthrough } ->
      let* calln = Value.try_loc (eval_vn target s.sto) in
      step_call p reserved_stack sp_diff calln fallthrough s
  | Jret -> (
      match s.stack with
      | [] -> Error (Format.asprintf "Empty stack")
      | (calln, sp_saved, retn') :: stack' ->
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
  | Jtailcall _ | Jtailcall_ind _ | Junimplemented -> Error "unimplemented jump"

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
