open StdlibExt
open Notation
module Value = NumericValue
module Action = StoreActionF.Make (Value)
module RegFile = RegFileF.Make (Value)
module Memory = DMemCombinedFailableMemory

type t = { regs : RegFile.t; mem : Memory.t }

let pp fmt v = Format.fprintf fmt "@[<1>regs: %a@]" RegFile.pp v.regs

let add_reg (s : t) (r : RegId.t_full) (v : Value.t) : t =
  { s with regs = RegFile.add_reg s.regs r v }

let get_reg (s : t) (r : RegId.t_full) : Value.t = RegFile.get_reg s.regs r

let load_mem (s : t) (v : Value.t) (width : Int32.t) :
    (Value.t, String.t) Result.t =
  let* addr = NumericValue.try_addr v in
  Memory.load_mem s.mem addr width

let load_string (s : t) (v : Value.t) : (String.t, String.t) Result.t =
  let* addr = NumericValue.try_addr v in
  Memory.load_string s.mem addr

let load_bytes (s : t) (v : Value.t) (width : Int32.t) :
    (String.t, String.t) Result.t =
  let* addr = NumericValue.try_addr v in
  Memory.load_bytes s.mem addr width

let store_mem (s : t) (v : Value.t) (v : Value.t) : (t, String.t) Result.t =
  let* addr = NumericValue.try_addr v in
  { s with mem = Memory.store_mem s.mem addr v } |> Result.ok

let store_bytes (s : t) (v : Value.t) (e : String.t) : (t, String.t) Result.t =
  let* addr = NumericValue.try_addr v in
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

let eval_vn_list (s : t) (vnl : VarNode.t List.t) :
    (Value.t List.t, String.t) Result.t =
  List.fold_right
    (fun vn acc ->
      match acc with
      | Ok acc ->
          let* v = eval_vn s vn in
          Ok (v :: acc)
      | Error e -> Error e)
    vnl (Ok [])

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

let step_IA (s : t) ({ expr; output } : IAssignment.t) :
    (Action.t, String.t) Result.t =
  let* v = eval_assignment s expr output.width in
  Ok (Action.Assign (output, v))

let step_ILS (s : t) (v : ILoadStore.t) : (Action.t, String.t) Result.t =
  match v with
  | Load { pointer; output } ->
      let* addr = eval_vn s pointer in
      let* v = load_mem s addr output.width in
      [%log debug "Loading %a from %a" Value.pp v Value.pp addr];
      Ok (Action.Load (output, addr, v))
  | Store { pointer; value } ->
      let* addr = eval_vn s pointer in
      let* v = eval_vn s value in
      [%log debug "Storing %a at %a" Value.pp v Value.pp addr];
      Ok (Action.Store (addr, v))

let step_IN (s : t) (_ : INop.t) : (Action.t, String.t) Result.t = Ok Action.Nop

let action_assign (s : t) (r : RegId.t_full) (v : Value.t) :
    (t, String.t) Result.t =
  Ok { s with regs = RegFile.add_reg s.regs r v }

let action_load (s : t) (r : RegId.t_full) (p : Value.t) (v : Value.t) :
    (t, String.t) Result.t =
  Ok { s with regs = RegFile.add_reg s.regs r v }

let action_store (s : t) (p : Value.t) (v : Value.t) : (t, String.t) Result.t =
  let* p = Value.try_addr p in
  Ok { s with mem = Memory.store_mem s.mem p v }

let action_nop (s : t) : (t, String.t) Result.t = Ok s

let action (s : t) (a : Action.t) =
  match a with
  | Assign (r, v) -> action_assign s r v
  | Load (r, p, v) -> action_load s r p v
  | Store (p, v) -> action_store s p v
  | Nop -> action_nop s

let build_arg (s : t) (tagv : Interop.tag) (v : Value.t) :
    (Interop.t, String.t) Result.t =
  match tagv with
  | TString ->
      let* v = load_string s v in
      Interop.VString v |> Result.ok
  | T8 ->
      let* v = Value.value_64 v in
      Interop.V8 (Char.chr (Int64.to_int v)) |> Result.ok
  | T16 ->
      let* v = Value.value_64 v in
      Interop.V16 (Int64.to_int32 v) |> Result.ok
  | T32 ->
      let* v = Value.value_64 v in
      Interop.V32 (Int64.to_int32 v) |> Result.ok
  | T64 ->
      let* v = Value.value_64 v in
      Interop.V64 v |> Result.ok
  | TBuffer n ->
      let* v = load_bytes s v (Int64.to_int32 n) in
      Interop.VBuffer (v |> String.to_bytes) |> Result.ok
  | TIBuffer n ->
      let* v = load_bytes s v (Int64.to_int32 n) in
      Interop.VIBuffer v |> Result.ok
  | _ -> "Not supported" |> Result.error

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
    ((Value.t * Interop.t) list, String.t) Result.t =
  if List.length fsig.params > 6 then
    [%log fatal "At most 6 argument is supported for external functions"];
  let reg_list = [ 56l; 48l; 16l; 8l; 128l; 136l ] in
  let val_list =
    List.map
      (fun r -> get_reg s { id = RegId.Register r; offset = 0l; width = 8l })
      reg_list
  in
  let* nondep_tags =
    List.fold_right
      (fun (tag : Interop.tag) (acc : (Interop.tag List.t, String.t) Result.t) ->
        let* ntag =
          match tag with
          | TBuffer_dep n ->
              let* k =
                build_arg s (List.nth fsig.params n) (List.nth val_list n)
              in
              Interop.TBuffer (Interop.extract_64 k) |> Result.ok
          | TIBuffer_dep n ->
              let* k =
                build_arg s (List.nth fsig.params n) (List.nth val_list n)
              in
              Interop.TIBuffer (Interop.extract_64 k) |> Result.ok
          | _ -> tag |> Result.ok
        in
        match acc with Ok l -> Ok (ntag :: l) | Error e -> Error e)
      fsig.params (Ok [])
  in
  try
    let ndt =
      List.combine nondep_tags (ListExt.take (List.length nondep_tags) val_list)
    in
    List.fold_right
      (fun (t, v) acc ->
        let* k = build_arg s t v in
        match acc with Ok acc -> Ok ((v, k) :: acc) | Error e -> Error e)
      ndt (Ok [])
  with Invalid_argument _ ->
    "Mismatched number of arguments for external functions" |> Result.error

let build_side (s : t) (value : Value.t) (t : Interop.t) :
    (t, String.t) Result.t =
  match t with
  | Interop.VBuffer v ->
      [%log debug "Storing extern_val at %a" Value.pp value];
      store_bytes s value (Bytes.to_string v)
  | _ -> Error "Unreachable"

let build_sides (s : t) (values : Value.t List.t)
    (sides : (Int.t * Interop.t) List.t) : (t, String.t) Result.t =
  List.fold_left
    (fun s (i, t) ->
      Result.bind s (fun s -> build_side s (List.nth values i) t))
    (Ok s) sides

let sp_extern (_ : 'a) : RegId.t_full =
  { id = RegId.Register 32l; offset = 0l; width = 8l }

let add_sp_extern (s : t) (_ : 'a) : (Value.t, String.t) Result.t =
  NumericBop.eval Bint_add (get_reg s (sp_extern ())) (Value.of_int64 8L 8l) 8l
