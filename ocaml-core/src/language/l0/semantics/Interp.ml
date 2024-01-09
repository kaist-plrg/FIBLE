open StdlibExt
open Basic
open Basic_collection

let ( let* ) = Result.bind

let eval_vn (vn : VarNode.t) (s : State.t) : Value.t =
  match vn with
  | Register r -> State.get_reg s r
  | Const v -> Value.of_int64 v.value v.width
  | Ram v -> State.load_mem s v.value v.width

let eval_assignment (a : Assignable.t) (s : State.t) (outwidth : Int32.t) :
    (Value.t, String.t) Result.t =
  match a with
  | Avar vn -> Ok (eval_vn vn s)
  | Auop (u, vn) -> Common_language.NumericUop.eval u (eval_vn vn s) outwidth
  | Abop (b, lv, rv) ->
      Common_language.NumericBop.eval b (eval_vn lv s) (eval_vn rv s) outwidth

let step_ins (p : Prog.t) (ins : Inst.t) (s : State.t) :
    (State.t, String.t) Result.t =
  match ins with
  | Iassignment { expr; output } ->
      let* v = eval_assignment expr s output.width in
      Ok
        {
          s with
          regs = RegFile.add_reg s.regs output v;
          pc = Prog.fallthru p s.pc;
        }
  | Iload { pointer; output; _ } ->
      let addr = eval_vn pointer s in
      let v = State.load_mem s (Value.to_addr addr) output.width in
      [%log debug "Loading %a from %a" Value.pp v Value.pp addr];
      Ok
        {
          s with
          regs = RegFile.add_reg s.regs output v;
          pc = Prog.fallthru p s.pc;
        }
  | Istore { pointer; value; _ } ->
      let addr = eval_vn pointer s in
      let v = eval_vn value s in
      [%log debug "Storing %a at %a" Value.pp v Value.pp addr];
      Ok
        {
          s with
          mem = Memory.store_mem s.mem (Value.to_addr addr) v;
          pc = Prog.fallthru p s.pc;
        }
  | Ijump l -> Ok { s with pc = l }
  | Icbranch { condition; target } ->
      let v = eval_vn condition s in
      if Value.isZero v then Ok { s with pc = Prog.fallthru p s.pc }
      else Ok { s with pc = target }
  | Ijump_ind vn ->
      let v = eval_vn vn s in
      Ok { s with pc = Value.to_loc v }
  | Inst.INop -> Ok { s with pc = Prog.fallthru p s.pc }
  | Inst.Iunimplemented -> Error "Unimplemented instruction"

let build_arg (s : State.t) (tagv : Common_language.Interop.tag) (v : Value.t) :
    Common_language.Interop.t =
  match tagv with
  | TString -> VString (Memory.load_string s.mem (Value.to_addr v))
  | T8 -> V8 (Char.chr (Int64.to_int (Value.value_64 v)))
  | T16 -> V16 (Int64.to_int32 (Value.value_64 v))
  | T32 -> V32 (Int64.to_int32 (Value.value_64 v))
  | T64 -> V64 (Value.value_64 v)
  | TFloat -> VFloat (Int64.float_of_bits (Value.value_64 v))
  | TDouble -> VDouble (Int64.float_of_bits (Value.value_64 v))
  | TVoid -> VUnit
  | TList _ -> [%log fatal "List not supported"]
  | TBuffer _ -> [%log fatal "Buffer not supported"]
  | TBuffer_dep _ -> [%log fatal "Buffer_dep not supported"]
  | TIBuffer _ -> [%log fatal "IBuffer not supported"]
  | TIBuffer_dep _ -> [%log fatal "IBuffer_dep not supported"]

let build_ret (s : State.t) (v : Common_language.Interop.t) : State.t =
  match v with
  | V8 c ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0l; offset = 0l; width = 8l }
            (Value.of_int64 (Int64.of_int (Char.code c)) 8l);
      }
  | V16 i ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0l; offset = 0l; width = 8l }
            (Value.of_int64 (Int64.of_int32 i) 8l);
      }
  | V32 i ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0l; offset = 0l; width = 8l }
            (Value.of_int64 (Int64.of_int32 i) 8l);
      }
  | V64 i ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0l; offset = 0l; width = 8l }
            (Value.of_int64 i 8l);
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
          State.get_reg s { id = RegId.Register reg; offset = 0l; width = 8l }
        in
        aux (build_arg s tag v :: acc) param_tags regs
    | _ -> [%log fatal "Not enough registers"]
  in
  aux [] fsig.params reg_list

let handle_extern (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr s.pc) p.externs with
  | None -> Ok s
  | Some name ->
      [%log debug "Calling %s" name];
      let retpointer =
        State.get_reg s { id = RegId.Register 32l; offset = 0l; width = 8l }
      in
      let retaddr = State.load_mem s (Value.to_addr retpointer) 8l in
      let* fsig, _ =
        StringMap.find_opt name World.Environment.signature_map
        |> Option.to_result ~none:"No external function"
      in
      let args = build_args s fsig in
      let _, retv = World.Environment.request_call name args in
      Ok
        (build_ret
           {
             s with
             pc = Value.to_loc retaddr;
             regs =
               RegFile.add_reg s.regs
                 { id = RegId.Register 32l; offset = 0l; width = 8l }
                 (Value.of_int64 (Int64.add (Value.value_64 retpointer) 8L) 8l);
           }
           retv)

let step (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  let* ins =
    Prog.get_ins p s.pc
    |> Option.to_result
         ~none:(Format.asprintf "No instruction at %a" Loc.pp s.pc)
  in
  let* ns = step_ins p ins s in
  handle_extern p ns

let rec interp (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  let s' = step p s in
  match s' with Error _ -> s' | Ok s' -> interp p s'
