module Make (VarNode : sig
  type t = NumericConst.t VarNodeF.poly_t

  val pp : Format.formatter -> t -> unit
end) (Assignable : sig
  type t = VarNode.t AssignableF.poly_t

  val pp : Format.formatter -> t -> unit
end) =
struct
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

  let store_bytes (s : t) (v : Value.t) (e : String.t) : (t, String.t) Result.t
      =
    let* addr = NumericValue.try_addr v in
    let mem = Memory.store_bytes s.mem addr e in
    { s with mem } |> Result.ok

  let alloc_string (mem : Memory.t) (arg : String.t) (sp : Int64.t) :
      Memory.t * Int64.t * Int64.t =
    let len = Int64.of_int ((String.length arg + 8) / 8 * 8) in
    let padlen = Int64.sub len (Int64.of_int (String.length arg)) in
    let arg = arg ^ String.make (Int64.to_int padlen) '\x00' in
    let nsp = Int64.sub sp len in
    let nmem = Memory.store_bytes mem nsp arg in
    (nmem, nsp, nsp)

  let alloc_strings (mem : Memory.t) (args : String.t List.t) (sp : Int64.t) :
      Memory.t * Int64.t * Int64.t List.t =
    let arg_rev = List.rev args in
    List.fold_left
      (fun (mem, sp, ptrs) arg ->
        let nmem, nsp, ptr = alloc_string mem arg sp in
        (nmem, nsp, ptr :: ptrs))
      (mem, sp, []) arg_rev

  let alloc_pointer (mem : Memory.t) (sp : Int64.t) (ptr : Int64.t) :
      Memory.t * Int64.t * Int64.t =
    let nsp = Int64.sub sp 8L in
    let nmem = Memory.store_mem mem nsp (Value.of_int64 ptr 8l) in
    (nmem, nsp, nsp)

  let alloc_array (mem : Memory.t) (sp : Int64.t) (ptrs : Int64.t List.t) :
      Memory.t * Int64.t * Int64.t =
    let ptrs = 0L :: List.rev ptrs in
    List.fold_left
      (fun (mem, sp, _) ptr ->
        let nmem, nsp, _ = alloc_pointer mem sp ptr in
        (nmem, nsp, nsp))
      (mem, sp, 0L) ptrs

  let init_libc_glob (v : t) (objects : (Int64.t * String.t) List.t)
      (argv_reg : Int32.t) : t =
    let swaped_objects = objects |> List.map (fun (a, b) -> (b, a)) in
    let argv =
      RegFile.get_reg v.regs
        { id = RegId.Register argv_reg; offset = 0l; width = 8l }
    in
    match
      let* argv_ptr = NumericValue.try_addr argv in
      let* argv0 = Memory.load_mem v.mem argv_ptr 8l in
      let* argv0_ptr = NumericValue.try_addr argv0 in
      let* progname = Memory.load_string v.mem argv0_ptr in
      let short_prog_idx =
        match String.rindex_opt progname '/' with
        | None -> 0L
        | Some idx -> Int64.of_int (idx + 1)
      in
      let* nmem =
        match List.assoc_opt "program_invocation_name" swaped_objects with
        | None -> v.mem |> Result.ok
        | Some addr ->
            let* addrp =
              NumericValue.of_int64 addr 8l |> NumericValue.try_addr
            in
            Memory.store_mem v.mem addrp argv0 |> Result.ok
      in
      let* nmem =
        match List.assoc_opt "program_invocation_short_name" swaped_objects with
        | None -> nmem |> Result.ok
        | Some addr ->
            let* addrp =
              NumericValue.of_int64 addr 8l |> NumericValue.try_addr
            in
            let* short_prog =
              NumericBop.eval Bint_add argv0
                (Value.of_num (NumericValue.of_int64 short_prog_idx 8l))
                8l
            in
            Memory.store_mem nmem addrp short_prog |> Result.ok
      in
      { v with mem = nmem } |> Result.ok
    with
    | Ok v -> v
    | Error s -> failwith s

  let init_build_stack (dmem : DMem.t) (init_sp : Int64.t)
      (args : String.t List.t) (envs : String.t List.t) (stack_size : Int64.t) :
      Memory.t * Int64.t * Int64.t * Int64.t =
    let mem_init = Memory.from_rom dmem in
    let nmem, nsp, envptrs =
      alloc_strings mem_init envs (Int64.add init_sp stack_size)
    in
    let nmem, nsp, argptrs = alloc_strings nmem args nsp in
    let nmem, nsp, envpptr = alloc_array nmem nsp envptrs in
    let nmem, nsp, argpptr = alloc_array nmem nsp argptrs in
    let nmem, nsp, _ = alloc_pointer nmem (Int64.add init_sp 8L) 0xDEADBEEFL in
    (nmem, nsp, argpptr, envpptr)

  let init_from_sig_libc (dmem : DMem.t) (rspec : int32 Int32Map.t)
      (init_sp : Int64.t) (mainaddr : Int64.t) (args : String.t List.t)
      (envs : String.t List.t) (stack_size : Int64.t) : t =
    let nmem, nsp, argpptr, envpptr =
      init_build_stack dmem init_sp args envs stack_size
    in
    let regs =
      RegFile.add_reg (RegFile.empty rspec)
        { id = RegId.Register 32l; offset = 0l; width = 8l }
        (Value.of_int64 nsp 8l)
    in
    let regs =
      RegFile.add_reg regs
        { id = RegId.Register 56l; offset = 0l; width = 8l }
        (Value.of_int64 mainaddr 8l)
    in
    let regs =
      RegFile.add_reg regs
        { id = RegId.Register 48l; offset = 0l; width = 8l }
        (Value.of_int64 (Int64.of_int (List.length args)) 8l)
    in
    let regs =
      RegFile.add_reg regs
        { id = RegId.Register 16l; offset = 0l; width = 8l }
        (Value.of_int64 argpptr 8l)
    in
    { regs; mem = nmem }

  let init_from_sig_main (dmem : DMem.t) (rspec : int32 Int32Map.t)
      (init_sp : Int64.t) (args : String.t List.t) (envs : String.t List.t)
      (stack_size : Int64.t) : t =
    let nmem, nsp, argpptr, envpptr =
      init_build_stack dmem init_sp args envs stack_size
    in
    let regs =
      RegFile.add_reg (RegFile.empty rspec)
        { id = RegId.Register 32l; offset = 0l; width = 8l }
        (Value.of_int64 nsp 8l)
    in
    let regs =
      RegFile.add_reg regs
        { id = RegId.Register 56l; offset = 0l; width = 8l }
        (Value.of_int64 (Int64.of_int (List.length args)) 8l)
    in
    let regs =
      RegFile.add_reg regs
        { id = RegId.Register 48l; offset = 0l; width = 8l }
        (Value.of_int64 argpptr 8l)
    in
    let regs =
      RegFile.add_reg regs
        { id = RegId.Register 16l; offset = 0l; width = 8l }
        (Value.of_int64 envpptr 8l)
    in
    { regs; mem = nmem }

  let eval_vn (s : t) (vn : VarNode.t) : (Value.t, String.t) Result.t =
    match vn with
    | Register r -> get_reg s r |> Result.ok
    | Const v -> Value.of_int64 v.value v.width |> Result.ok
    | Ram (v, width) -> load_mem s (Value.of_int64 v.value 8l) width

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

  let step_IA (s : t)
      ({ expr; output } : VarNode.t AssignableF.poly_t IAssignment.poly_t) :
      (Action.t, String.t) Result.t =
    let* v = eval_assignment s expr output.width in
    Ok (Action.Assign (output, v))

  let step_ILS (s : t) (v : VarNode.t ILoadStore.poly_t) :
      (Action.t, String.t) Result.t =
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

  let step_IN (s : t) (_ : INop.t) : (Action.t, String.t) Result.t =
    Ok Action.Nop

  let action_assign (s : t) (r : RegId.t_full) (v : Value.t) :
      (t, String.t) Result.t =
    Ok { s with regs = RegFile.add_reg s.regs r v }

  let action_load (s : t) (r : RegId.t_full) (p : Value.t) (v : Value.t) :
      (t, String.t) Result.t =
    Ok { s with regs = RegFile.add_reg s.regs r v }

  let action_store (s : t) (p : Value.t) (v : Value.t) : (t, String.t) Result.t
      =
    let* p = Value.try_addr p in
    Ok { s with mem = Memory.store_mem s.mem p v }

  let action_nop (s : t) : (t, String.t) Result.t = Ok s

  let interop_interface : (t, Value.t) Interop.interface_t =
    {
      load_mem;
      load_bytes;
      store_bytes;
      try_num = Result.ok;
      add_num =
        (fun (a : Value.t) (b : Int64.t) ->
          NumericBop.eval Bint_add a (Value.of_int64 b 8l) 8l);
    }

  let interop_accessor : (t, Value.t) Interop.acccessor_t =
    Interop.get_accessor interop_interface

  let build_arg :
      t ->
      Interop.prim_tag ->
      Value.t ->
      (Value.t Interop.side_t * Interop.t, String.t) Result.t =
    interop_accessor.prim_acc
  (*
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
     | _ -> "Not supported" |> Result.error *)

  let build_ret (s : t) (v : Interop.t) : (t, String.t) Result.t =
    let rv = Interop.extract_64 v in
    (* match v with
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
    *)
    add_reg s
      { id = RegId.Register 0l; offset = 0l; width = 8l }
      (Value.of_int64 rv 8l)
    |> Result.ok

  let build_args (s : t) (fsig : Interop.func_sig) :
      ((Value.t * Bytes.t) list * Interop.t list, String.t) Result.t =
    if List.length (snd fsig.params) > 6 then
      [%log fatal "At most 6 argument is supported for external functions"];
    let reg_list = [ 56l; 48l; 16l; 8l; 128l; 136l ] in
    let val_list =
      List.map
        (fun r -> get_reg s { id = RegId.Register r; offset = 0l; width = 8l })
        reg_list
    in
    let val_list = List.take (List.length (snd fsig.params)) val_list in
    interop_accessor.fsig_acc s fsig val_list

  let build_side (s : t) (value : Value.t) (t : Bytes.t) :
      (t, String.t) Result.t =
    [%log debug "Storing extern_val at %a" Value.pp value];
    store_bytes s value (t |> Bytes.to_string)

  let build_sides (s : t) (sides : (Value.t * Bytes.t) List.t) :
      (t, String.t) Result.t =
    List.fold_left
      (fun s (i, t) -> Result.bind s (fun s -> build_side s i t))
      (Ok s) sides

  let step_SP (syscall_table : Int64.t -> Interop.func_sig Option.t) (s : t)
      (name : ISpecial.t) : (Action.t, String.t) Result.t =
    match name with
    | "syscall" ->
        let* _, rax =
          build_arg s Interop.t64
            (get_reg s { id = RegId.Register 0l; offset = 0l; width = 8l })
        in
        let* snum =
          match rax with
          | VArith (VInt (V64 rax)) -> rax |> Result.ok
          | _ -> "rax is not a 64-bit integer" |> Result.error
        in
        let* fsig =
          match syscall_table snum with
          | Some fsig -> fsig |> Result.ok
          | None -> Format.sprintf "syscall %Ld not found" snum |> Result.error
        in
        let* sides, args = build_args s fsig in
        Ok (Action.Special ("syscall", sides, rax :: args))
    | _ -> Ok (Action.Special (name, [], []))

  let sp_extern (_ : 'a) : RegId.t_full =
    { id = RegId.Register 32l; offset = 0l; width = 8l }

  let add_sp_extern (s : t) (_ : 'a) : (Value.t, String.t) Result.t =
    NumericBop.eval Bint_add
      (get_reg s (sp_extern ()))
      (Value.of_int64 8L 8l) 8l

  let get_regfile (s : t) = s.regs
end
