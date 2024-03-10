open StdlibExt
open Notation
open Basic
open Basic_collection
module Value = NumericValue
module RegFile = RegFileF.Make (NumericValue)
module Memory = DMemCombinedMemory

type t = { regs : RegFile.t; mem : Memory.t }

let add_reg (s : t) (r : RegId.t_full) (v : Value.t) : t =
  { s with regs = RegFile.add_reg s.regs r v }

let get_reg (s : t) (r : RegId.t_full) : Value.t = RegFile.get_reg s.regs r

let load_mem (s : t) (v : Value.t) (width : Int32.t) :
    (Value.t, String.t) Result.t =
  let addr = NumericValue.to_addr v in
  Memory.load_mem s.mem addr width |> Result.ok

let load_string (s : t) (v : Value.t) : (String.t, String.t) Result.t =
  let addr = NumericValue.to_addr v in
  Memory.load_string s.mem addr |> Result.ok

let load_bytes (s : t) (v : Value.t) (width : Int32.t) :
    (String.t, String.t) Result.t =
  let addr = NumericValue.to_addr v in
  Memory.load_bytes s.mem addr width |> Result.ok

let store_mem (s : t) (v : Value.t) (v : Value.t) : (t, String.t) Result.t =
  let addr = NumericValue.to_addr v in
  { s with mem = Memory.store_mem s.mem addr v } |> Result.ok

let store_bytes (s : t) (v : Value.t) (e : String.t) : (t, String.t) Result.t =
  let addr = NumericValue.to_addr v in
  let mem = Memory.store_bytes s.mem addr e in
  { s with mem } |> Result.ok

let init_from_sig (dmem : DMem.t) (rspec : int32 Int32Map.t) (init_sp : Int64.t)
    : t =
  let regs =
    RegFile.add_reg (RegFile.empty rspec)
      { id = RegId.Register 32l; offset = 0l; width = 8l }
      (Value.of_int64 init_sp 8l)
  in
  let mem =
    Memory.store_mem (Memory.from_rom dmem) init_sp
      (Value.of_int64 0xDEADBEEFL 8l)
  in
  { regs; mem }

let eval_vn (s : t) (vn : VarNode.t) : (Value.t, String.t) Result.t =
  match vn with
  | Register r -> get_reg s r |> Result.ok
  | Const v -> Value.of_int64 v.value v.width |> Result.ok
  | Ram v -> load_mem s (Value.of_int64 v.value 8l) v.width

let eval_assignment (s : t) (a : Assignable.t) (outwidth : Int32.t) :
    (Value.t, String.t) Result.t =
  match a with
  | Avar vn -> eval_vn s vn
  | Auop (u, vn) ->
      let* v = eval_vn s vn in
      NumericUop.eval u v outwidth
  | Abop (b, lv, rv) ->
      let* lv = eval_vn s lv in
      let* rv = eval_vn s rv in
      NumericBop.eval b lv rv outwidth

let step_IA (s : t) ({ expr; output } : IAssignment.t) : (t, String.t) Result.t
    =
  let* v = eval_assignment s expr output.width in
  Ok { s with regs = RegFile.add_reg s.regs output v }

let step_ILS (s : t) (v : ILoadStore.t) : (t, String.t) Result.t =
  match v with
  | Load { pointer; output } ->
      let* addr = eval_vn s pointer in
      let* v = load_mem s addr output.width in
      [%log debug "Loading %a from %a" Value.pp v Value.pp addr];
      Ok { s with regs = RegFile.add_reg s.regs output v }
  | Store { pointer; value } ->
      let* addr = eval_vn s pointer in
      let* v = eval_vn s value in
      [%log debug "Storing %a at %a" Value.pp v Value.pp addr];
      Ok { s with mem = Memory.store_mem s.mem (Value.to_addr addr) v }

let step_IN (s : t) (_ : INop.t) : (t, String.t) Result.t = Ok s

let build_arg (s : t) (tagv : Interop.tag) (v : Value.t) :
    (Interop.t, String.t) Result.t =
  match tagv with
  | TString ->
      let* s = load_string s v in
      Interop.VString s |> Result.ok
  | T8 -> Interop.V8 (Char.chr (Int64.to_int (Value.value_64 v))) |> Result.ok
  | T16 -> Interop.V16 (Int64.to_int32 (Value.value_64 v)) |> Result.ok
  | T32 -> Interop.V32 (Int64.to_int32 (Value.value_64 v)) |> Result.ok
  | T64 -> Interop.V64 (Value.value_64 v) |> Result.ok
  | TFloat ->
      Interop.VFloat (Int64.float_of_bits (Value.value_64 v)) |> Result.ok
  | TDouble ->
      Interop.VDouble (Int64.float_of_bits (Value.value_64 v)) |> Result.ok
  | TVoid -> Interop.VUnit |> Result.ok
  | TList _ -> "List not supported" |> Result.error
  | TBuffer _ -> "Buffer not supported" |> Result.error
  | TBuffer_dep _ -> "Buffer_dep not supported" |> Result.error
  | TIBuffer _ -> "IBuffer not supported" |> Result.error
  | TIBuffer_dep _ -> "IBuffer_dep not supported" |> Result.error

let build_ret (s : t) (v : Interop.t) : (t, String.t) Result.t =
  match v with
  | V8 c ->
      add_reg s
        { id = RegId.Register 0l; offset = 0l; width = 8l }
        (Value.of_int64 (Int64.of_int (Char.code c)) 8l)
      |> Result.ok
  | V16 i ->
      add_reg s
        { id = RegId.Register 0l; offset = 0l; width = 8l }
        (Value.of_int64 (Int64.of_int32 i) 8l)
      |> Result.ok
  | V32 i ->
      add_reg s
        { id = RegId.Register 0l; offset = 0l; width = 8l }
        (Value.of_int64 (Int64.of_int32 i) 8l)
      |> Result.ok
  | V64 i ->
      add_reg s
        { id = RegId.Register 0l; offset = 0l; width = 8l }
        (Value.of_int64 i 8l)
      |> Result.ok
  | _ -> "Unsupported return type" |> Result.error

let build_args (s : t) (fsig : Interop.func_sig) :
    (Interop.t list, String.t) Result.t =
  if List.length fsig.params > 6 then
    "At most 6 argument is supported for external functions" |> Result.error
  else
    let reg_list = [ 56l; 48l; 16l; 8l; 128l; 136l ] in
    let rec aux (acc : Interop.t list) (param_tags : Interop.tag list)
        (regs : Int32.t list) : (Interop.t list, String.t) Result.t =
      match (param_tags, regs) with
      | [], _ -> List.rev acc |> Result.ok
      | tag :: param_tags, reg :: regs ->
          let v =
            get_reg s { id = RegId.Register reg; offset = 0l; width = 8l }
          in
          let* x = build_arg s tag v in
          aux (x :: acc) param_tags regs
      | _ -> "Not enough registers" |> Result.error
    in
    aux [] fsig.params reg_list
