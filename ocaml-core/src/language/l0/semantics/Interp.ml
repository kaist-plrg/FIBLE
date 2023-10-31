open StdlibExt
open Basic
open Basic_collection

let ( let* ) = Result.bind

let eval_vn (vn : VarNode.t) (s : State.t) : Value.t =
  match vn with
  | Register r -> State.get_reg s r
  | Const v -> { value = v.value; width = v.width }

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
  | Iassignment (a, o) ->
      let* v = eval_assignment a s o.width in
      Ok { s with regs = RegFile.add_reg s.regs o v; pc = Prog.fallthru p s.pc }
  | Iload (_, addrvn, outputid) ->
      let addr = eval_vn addrvn s in
      let v = State.load_mem s (Value.to_addr addr) outputid.width in
      [%log debug "Loading %a from %a" Value.pp v Value.pp addr];
      Ok
        {
          s with
          regs = RegFile.add_reg s.regs outputid v;
          pc = Prog.fallthru p s.pc;
        }
  | Istore (_, addrvn, valuevn) ->
      let addr = eval_vn addrvn s in
      let v = eval_vn valuevn s in
      [%log debug "Storing %a at %a" Value.pp v Value.pp addr];
      Ok
        {
          s with
          mem = Memory.store_mem s.mem (Value.to_addr addr) v;
          pc = Prog.fallthru p s.pc;
        }
  | Ijump l -> Ok { s with pc = l }
  | Icbranch (vn, l) ->
      let v = eval_vn vn s in
      if Value.isZero v then Ok { s with pc = Prog.fallthru p s.pc }
      else Ok { s with pc = l }
  | Ijump_ind vn ->
      let v = eval_vn vn s in
      Ok { s with pc = Value.to_loc v }
  | Inst.INop -> Ok { s with pc = Prog.fallthru p s.pc }
  | Inst.Iunimplemented -> Error "Unimplemented instruction"

let build_arg (s : State.t) (tagv : Common_language.Interop.tag) (v : Value.t) :
    Common_language.Interop.t =
  match tagv with
  | TString -> VString (Memory.load_string s.mem (Value.to_addr v))
  | T8 -> V8 (Char.chr (Int64.to_int v.value))
  | T16 -> V16 (Int64.to_int32 v.value)
  | T32 -> V32 (Int64.to_int32 v.value)
  | T64 -> V64 v.value
  | TFloat -> VFloat (Int64.float_of_bits v.value)
  | TDouble -> VDouble (Int64.float_of_bits v.value)
  | TVoid -> VUnit
  | TList _ -> failwith "List not supported"

let build_ret (s : State.t) (v : Common_language.Interop.t) : State.t =
  match v with
  | V8 c ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0L; width = 8l }
            { value = Int64.of_int (Char.code c); width = 8l };
      }
  | V16 i ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0L; width = 8l }
            { value = Int64.of_int32 i; width = 8l };
      }
  | V32 i ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0L; width = 8l }
            { value = Int64.of_int32 i; width = 8l };
      }
  | V64 i ->
      {
        s with
        regs =
          RegFile.add_reg s.regs
            { id = RegId.Register 0L; width = 8l }
            { value = i; width = 8l };
      }
  | _ -> failwith "Unsupported return type"

let build_args (s : State.t) (fsig : Common_language.Interop.func_sig) :
    Common_language.Interop.t list =
  if List.length fsig.params > 6 then
    failwith "At most 6 argument is supported for external functions";
  let reg_list = [ 56L; 48L; 16L; 8L; 128L; 136L ] in
  let rec aux (acc : Common_language.Interop.t list)
      (param_tags : Common_language.Interop.tag list) (regs : Int64.t list) :
      Common_language.Interop.t list =
    match (param_tags, regs) with
    | [], _ -> List.rev acc
    | tag :: param_tags, reg :: regs ->
        let v = State.get_reg s { id = RegId.Register reg; width = 8l } in
        aux (build_arg s tag v :: acc) param_tags regs
    | _ -> failwith "Not enough registers"
  in
  aux [] fsig.params reg_list

let handle_extern (p : Prog.t) (s : State.t) : (State.t, String.t) Result.t =
  match AddrMap.find_opt (Loc.to_addr s.pc) p.externs with
  | None -> Ok s
  | Some name ->
      [%log debug "Calling %s" name];
      let retpointer =
        State.get_reg s { id = RegId.Register 32L; width = 8l }
      in
      let retaddr = State.load_mem s (Value.to_addr retpointer) 8l in
      let* fsig, _ =
        StringMap.find_opt name World.Environment.signature_map
        |> Option.to_result ~none:"No external function"
      in
      let args = build_args s fsig in
      let retv = World.Environment.request_call name args in
      Ok
        (build_ret
           {
             s with
             pc = Value.to_loc retaddr;
             regs =
               RegFile.add_reg s.regs
                 { id = RegId.Register 32L; width = 8l }
                 { value = Int64.add retpointer.value 8L; width = 8l };
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
