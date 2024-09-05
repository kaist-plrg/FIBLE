module Make
    (Prog : sig
      type t

      val sp_num : t -> Int32.t
    end)
    (Const : ConstF.S)
    (VarNode : VarNodeF.S with module Const = Const)
    (Pointer : PointerF.S)
    (Value : ValueF.S with module Const = Const and module Pointer = Pointer)
    (SPtoVal : sig
      type t = Value.t

      val sp : SPVal.t -> Value.t
    end)
    (Action : sig
      type t

      val of_assign : RegId.t_full -> Value.t -> t
      val of_load : RegId.t_full -> Value.t -> Value.t -> t
      val of_store : Value.t -> Value.t -> t

      val of_special :
        String.t -> (Value.t * Bytes.t) List.t -> Interop.t List.t -> t

      val nop : t

      val to_either5 :
        t ->
        ( RegId.t_full * Value.t,
          RegId.t_full * Value.t * Value.t,
          Value.t * Value.t,
          String.t * (Value.t * Bytes.t) List.t * Interop.t List.t,
          Unit.t )
        Either5.t
    end)
    (HighCursor : sig
      type t

      val get_func_loc : t -> Loc.t
      val get_timestamp : t -> Int64.t
    end)
    (RegFile : sig
      type t

      val empty : int32 Int32Map.t -> t
      val pp : Format.formatter -> t -> unit
      val add_reg : t -> RegId.t_full -> Value.t -> t
      val get_reg : t -> RegId.t_full -> Value.t
    end)
    (Memory : MemoryF.S with module Value = Value and module Pointer = Pointer)
    (Frame : sig
      type t

      val empty : Int64.t Option.t -> Int64.t -> t
      val store_mem : t -> Int64.t -> Value.t -> (t, String.t) Result.t
    end) =
struct
  type t = { regs : RegFile.t; mem : Memory.t }

  let pp fmt v = Format.fprintf fmt "@[<1>regs: %a@]" RegFile.pp v.regs

  let add_reg (s : t) (r : RegId.t_full) (v : Value.t) : t =
    { s with regs = RegFile.add_reg s.regs r v }

  let get_reg (s : t) (r : RegId.t_full) : Value.t = RegFile.get_reg s.regs r

  let load_mem (s : t) (v : Value.t) (width : Int32.t) :
      (Value.t, String.t) Result.t =
    let* ptv = Value.try_pointer v in
    Memory.load_mem s.mem ptv width |> Result.ok

  let load_string (s : t) (v : Value.t) : (String.t, String.t) Result.t =
    let* ptv = Value.try_pointer v in
    Memory.load_string s.mem ptv

  let load_bytes (s : t) (v : Value.t) (width : Int32.t) :
      (String.t, String.t) Result.t =
    let* ptv = Value.try_pointer v in
    Memory.load_bytes s.mem ptv width

  let store_mem (s : t) (v : Value.t) (e : Value.t) : (t, String.t) Result.t =
    let* ptv = Value.try_pointer v in
    let* mem = Memory.store_mem s.mem ptv e in
    { s with mem } |> Result.ok

  let store_bytes (s : t) (v : Value.t) (e : String.t) : (t, String.t) Result.t
      =
    let* ptv = Value.try_pointer v in
    let* mem = Memory.store_bytes s.mem ptv e in
    { s with mem } |> Result.ok

  let alloc_string (mem : Memory.t) (arg : String.t) (sp : Value.t) :
      (Memory.t * Value.t * Value.t, String.t) Result.t =
    let len = Int64.of_int ((String.length arg + 1 + 7) / 8 * 8) in
    let padlen = Int64.sub len (Int64.of_int (String.length arg)) in
    let arg = arg ^ String.make (Int64.to_int padlen) '\x00' in
    let* nsp =
      Value.eval_bop Bint_sub sp
        (Value.of_num (NumericValue.of_int64 len 8l))
        8l
    in
    let* spptr = Value.try_pointer nsp in
    let* nmem = Memory.store_bytes mem spptr arg in
    (nmem, nsp, nsp) |> Result.ok

  let alloc_strings (mem : Memory.t) (args : String.t List.t) (sp : Value.t) :
      (Memory.t * Value.t * Value.t List.t, String.t) Result.t =
    let arg_rev = List.rev args in
    Result.fold_left_M
      (fun (mem, sp, ptrs) arg ->
        let* nmem, nsp, ptr = alloc_string mem arg sp in
        (nmem, nsp, ptr :: ptrs) |> Result.ok)
      (mem, sp, []) arg_rev

  let alloc_pointer (mem : Memory.t) (sp : Value.t) (ptr : Value.t) :
      (Memory.t * Value.t * Value.t, String.t) Result.t =
    let* nsp =
      Value.eval_bop Bint_sub sp (Value.of_num (NumericValue.of_int64 8L 8l)) 8l
    in
    let* spptr = Value.try_pointer nsp in
    let* nmem = Memory.store_mem mem spptr ptr in
    (nmem, nsp, nsp) |> Result.ok

  let alloc_array (mem : Memory.t) (sp : Value.t) (ptrs : Value.t List.t) :
      (Memory.t * Value.t * Value.t, String.t) Result.t =
    let ptrs = Value.zero 8l :: List.rev ptrs in
    Result.fold_left_M
      (fun (mem, sp, _) ptr ->
        let* nmem, nsp, _ = alloc_pointer mem sp ptr in
        (nmem, nsp, nsp) |> Result.ok)
      (mem, sp, Value.zero 8l)
      ptrs

  let init_libc_glob (v : t) (objects : (Int64.t * String.t) List.t) : t =
    let swaped_objects = objects |> List.map (fun (a, b) -> (b, a)) in
    let argv =
      RegFile.get_reg v.regs
        { id = RegId.Register 48l; offset = 0l; width = 8l }
    in
    match
      let* argv_ptr = Value.try_pointer argv in
      let argv0 = Memory.load_mem v.mem argv_ptr 8l in
      let* argv0_ptr = Value.try_pointer argv0 in
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
              Value.of_num (NumericValue.of_int64 addr 8l) |> Value.try_pointer
            in
            Memory.store_mem v.mem addrp argv0
      in
      let* nmem =
        match List.assoc_opt "program_invocation_short_name" swaped_objects with
        | None -> nmem |> Result.ok
        | Some addr ->
            let* addrp =
              Value.of_num (NumericValue.of_int64 addr 8l) |> Value.try_pointer
            in
            let* short_prog =
              Value.eval_bop Bint_add argv0
                (Value.of_num (NumericValue.of_int64 short_prog_idx 8l))
                8l
            in
            Memory.store_mem nmem addrp short_prog
      in
      { v with mem = nmem } |> Result.ok
    with
    | Ok v -> v
    | Error s -> failwith s

  let init_build_stack (dmem : DMem.t) (frame_addr : Loc.t * Memory.TimeStamp.t)
      (init_frame : Memory.Frame.t) (init_sp : Value.t) (args : String.t List.t)
      (envs : String.t List.t) :
      (Memory.t * Value.t * Value.t * Value.t, String.t) Result.t =
    let mem_init =
      Memory.of_global_memory (Memory.GlobalMemory.from_rom dmem)
      |> Memory.add_local_frame frame_addr init_frame
    in
    let* sp_start =
      Value.eval_bop Bint_add init_sp
        (Value.of_num (NumericValue.of_int64 4096L 8l))
        8l
    in
    let* sp_final =
      Value.eval_bop Bint_add init_sp
        (Value.of_num (NumericValue.of_int64 8L 8l))
        8l
    in
    let* nmem, nsp, envptrs = alloc_strings mem_init envs sp_start in
    let* nmem, nsp, argptrs = alloc_strings nmem args nsp in
    let* nmem, nsp, envpptr = alloc_array nmem nsp envptrs in
    let* nmem, nsp, argpptr = alloc_array nmem nsp argptrs in
    let* nmem, nsp, _ =
      alloc_pointer nmem sp_final
        (Value.of_num (NumericValue.of_int64 0xDEADBEEFL 8l))
    in
    (nmem, nsp, argpptr, envpptr) |> Result.ok

  let init_from_sig_libc (dmem : DMem.t) (rspec : int32 Int32Map.t)
      (frame_addr : Loc.t * Memory.TimeStamp.t) (init_frame : Memory.Frame.t)
      (init_sp : Value.t) (mainaddr : Int64.t) (args : String.t List.t)
      (envs : String.t List.t) : t =
    let nmem, nsp, argpptr, envpptr =
      match init_build_stack dmem frame_addr init_frame init_sp args envs with
      | Ok s -> s
      | Error s -> [%log error "%s" s]
    in
    let regs =
      RegFile.add_reg (RegFile.empty rspec)
        { id = RegId.Register 32l; offset = 0l; width = 8l }
        nsp
    in
    let regs =
      RegFile.add_reg regs
        { id = RegId.Register 56l; offset = 0l; width = 8l }
        (Value.of_num (NumericValue.of_int64 mainaddr 8l))
    in
    let regs =
      RegFile.add_reg regs
        { id = RegId.Register 48l; offset = 0l; width = 8l }
        (Value.of_num
           (NumericValue.of_int64 (Int64.of_int (List.length args)) 8l))
    in
    let regs =
      RegFile.add_reg regs
        { id = RegId.Register 16l; offset = 0l; width = 8l }
        argpptr
    in
    { regs; mem = nmem }

  let init_from_sig_main (dmem : DMem.t) (rspec : int32 Int32Map.t)
      (frame_addr : Loc.t * Memory.TimeStamp.t) (init_frame : Memory.Frame.t)
      (init_sp : Value.t) (args : String.t List.t) (envs : String.t List.t) : t
      =
    let nmem, nsp, argpptr, envpptr =
      init_build_stack dmem frame_addr init_frame init_sp args envs
      |> Result.get_ok
    in
    let v =
      let regs =
        RegFile.add_reg (RegFile.empty rspec)
          { id = RegId.Register 32l; offset = 0l; width = 8l }
          nsp
      in
      let regs =
        RegFile.add_reg regs
          { id = RegId.Register 56l; offset = 0l; width = 8l }
          (Value.of_num
             (NumericValue.of_int64 (Int64.of_int (List.length args)) 8l))
      in
      let regs =
        RegFile.add_reg regs
          { id = RegId.Register 48l; offset = 0l; width = 8l }
          argpptr
      in
      let regs =
        RegFile.add_reg regs
          { id = RegId.Register 16l; offset = 0l; width = 8l }
          envpptr
      in
      { regs; mem = nmem } |> Result.ok
    in
    match v with Ok v -> v | Error s -> failwith s

  let eval_vn (s : t) (vn : VarNode.t) : (Value.t, String.t) Result.t =
    match vn with
    | Register r -> get_reg s r |> Result.ok
    | Const v -> v |> Value.of_const |> Result.ok
    | Ram (v, width) -> load_mem s (v |> Value.of_const) width

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

  let eval_assignment (s : t) (a : VarNode.t AssignableF.poly_t)
      (outwidth : Int32.t) : (Value.t, String.t) Result.t =
    match a with
    | Avar vn -> eval_vn s vn
    | Auop (u, vn) ->
        let* v = eval_vn s vn in
        Value.eval_uop u v outwidth
    | Abop (Bop.Bint_xor, lv, rv) when VarNode.compare lv rv = 0 ->
        Value.zero outwidth |> Result.ok
    | Abop (Bop.Bint_sub, lv, rv) when VarNode.compare lv rv = 0 ->
        Value.zero outwidth |> Result.ok
    | Abop (b, lv, rv) ->
        let* lv = eval_vn s lv in
        let* rv = eval_vn s rv in
        Value.eval_bop b lv rv outwidth

  let step_IA (s : t)
      ({ expr; output } : VarNode.t AssignableF.poly_t IAssignment.poly_t) :
      (Action.t, String.t) Result.t =
    let* v = eval_assignment s expr output.width in
    Action.of_assign output v |> Result.ok

  let step_ILS (s : t) (v : VarNode.t ILoadStore.poly_t) :
      (Action.t, String.t) Result.t =
    match v with
    | Load { pointer; output; _ } ->
        let* addrv = eval_vn s pointer in
        let* lv = load_mem s addrv output.width in
        [%log debug "Loading %a from %a" Value.pp lv Value.pp addrv];
        Action.of_load output addrv lv |> Result.ok
    | Store { pointer; value; _ } ->
        let* addrv = eval_vn s pointer in
        let* sv = eval_vn s value in
        [%log debug "Storing %a at %a" Value.pp sv Value.pp addrv];
        Action.of_store addrv sv |> Result.ok

  let step_ISLS (s : t) (curr : HighCursor.t) (v : VarNode.t ISLoadStore.poly_t)
      : (Action.t, String.t) Result.t =
    match v with
    | Sload { offset; output } ->
        let addrv =
          SPtoVal.sp
            {
              func = HighCursor.get_func_loc curr;
              timestamp = HighCursor.get_timestamp curr;
              multiplier = 1L;
              offset;
            }
        in
        let* lv = load_mem s addrv output.width in
        [%log debug "Loading %a from %a" Value.pp lv Value.pp addrv];
        Action.of_load output addrv lv |> Result.ok
    | Sstore { offset; value } ->
        let addrv =
          SPtoVal.sp
            {
              func = HighCursor.get_func_loc curr;
              timestamp = HighCursor.get_timestamp curr;
              multiplier = 1L;
              offset;
            }
        in
        let* sv = eval_vn s value in
        [%log debug "Storing %a at %a" Value.pp sv Value.pp addrv];
        Action.of_store addrv sv |> Result.ok

  let step_IN (s : t) (_ : INop.t) : (Action.t, String.t) Result.t =
    Ok Action.nop

  let action_assign (s : t) (r : RegId.t_full) (v : Value.t) :
      (t, String.t) Result.t =
    add_reg s r v |> Result.ok

  let action_load (s : t) (r : RegId.t_full) (p : Value.t) (v : Value.t) :
      (t, String.t) Result.t =
    add_reg s r v |> Result.ok

  let action_store (s : t) (p : Value.t) (v : Value.t) : (t, String.t) Result.t
      =
    store_mem s p v

  let action_nop (s : t) : (t, String.t) Result.t = Ok s

  let interop_interface : (t, Value.t) Interop.interface_t =
    {
      load_mem;
      load_bytes;
      store_bytes;
      try_num = Value.try_num;
      add_num =
        (fun (a : Value.t) (b : Int64.t) ->
          Value.eval_bop Bint_add a
            (Value.of_num (NumericValue.of_int64 b 8l))
            8l);
    }

  let interop_accessor : (t, Value.t) Interop.acccessor_t =
    Interop.get_accessor interop_interface

  let build_arg :
      t ->
      Interop.prim_tag ->
      Value.t ->
      (Value.t Interop.side_t * Interop.t, String.t) Result.t =
    interop_accessor.prim_acc
  (* match tagv with
     | TString ->
         let* v = load_string s v in
         Interop.VString v |> Result.ok
     | T8 -> (
         match Value.try_num v with
         | Ok value ->
             let* value = NumericValue.value_64 value in
             Interop.V8 (Char.chr (Int64.to_int value)) |> Result.ok
         | _ -> Error "Not a number")
     | T16 -> (
         match Value.try_num v with
         | Ok value ->
             let* value = NumericValue.value_64 value in
             Interop.V16 (Int64.to_int32 value) |> Result.ok
         | _ -> Error "Not a number")
     | T32 -> (
         match Value.try_num v with
         | Ok value ->
             let* value = NumericValue.value_64 value in

             Interop.V32 (Int64.to_int32 value) |> Result.ok
         | _ -> Error "Not a number")
     | T64 -> (
         match Value.try_num v with
         | Ok value ->
             let* value = NumericValue.value_64 value in
             Interop.V64 value |> Result.ok
         | _ ->
             Interop.V64
               (Foreign.foreign "strdup"
                  (Ctypes_static.( @-> ) Ctypes.string
                     (Ctypes.returning Ctypes_static.int64_t))
                  "[null]")
             |> Result.ok)
     | TBuffer n ->
         let* v = load_bytes s v (Int64.to_int32 n) in
         Interop.VBuffer (v |> String.to_bytes) |> Result.ok
     | TIBuffer n ->
         let* v = load_bytes s v (Int64.to_int32 n) in
         Interop.VIBuffer v |> Result.ok
     | _ -> "Not supported" |> Result.error *)

  let build_ret (s : t) (v : Interop.t) : (t, String.t) Result.t =
    let rv = Interop.extract_64 v in
    (*
    match v with
    | V8 c ->
        add_reg s
          { id = RegId.Register 0l; offset = 0l; width = 8l }
          (Value.of_num (NumericValue.of_int64 (Int64.of_int (Char.code c)) 8l))
        |> Result.ok
    | V16 i ->
        add_reg s
          { id = RegId.Register 0l; offset = 0l; width = 8l }
          (Value.of_num (NumericValue.of_int64 (Int64.of_int32 i) 8l))
        |> Result.ok
    | V32 i ->
        add_reg s
          { id = RegId.Register 0l; offset = 0l; width = 8l }
          (Value.of_num (NumericValue.of_int64 (Int64.of_int32 i) 8l))
        |> Result.ok
    | V64 i ->
        add_reg s
          { id = RegId.Register 0l; offset = 0l; width = 8l }
          (Value.of_num (NumericValue.of_int64 i 8l))
        |> Result.ok
    | _ -> "Unsupported return type" |> Result.error *)
    add_reg s
      { id = RegId.Register 0l; offset = 0l; width = 8l }
      (Value.of_num (NumericValue.of_int64 rv 8l))
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
        [%log finfo "syscall" "SYSCALL NUM: %Ld" snum];
        let* fsig =
          match syscall_table snum with
          | Some fsig -> fsig |> Result.ok
          | None -> Format.sprintf "syscall %Ld not found" snum |> Result.error
        in
        let* sides, args = build_args s fsig in
        Ok (Action.of_special "syscall" sides (rax :: args))
    | _ -> Ok (Action.of_special name [] [])

  let get_sp_curr (s : t) (p : Prog.t) : Value.t =
    get_reg s { id = RegId.Register (Prog.sp_num p); offset = 0l; width = 8l }

  let build_local_frame (s : t) (p : Prog.t) (bnd : Int64.t Option.t * Int64.t)
      (copydepth : Int64.t) =
    let sp_curr = get_sp_curr s p in
    let* passing_vals =
      List.fold_left
        (fun acc (i, x) ->
          match acc with
          | Error _ -> acc
          | Ok acc ->
              let* addr = x in
              let* v = load_mem s addr 8l in
              Ok ((i, v) :: acc))
        (Ok [])
        (Int64.div (Int64.add copydepth 7L) 8L
        |> Int64.to_int
        |> Fun.flip List.init (fun x ->
               ( Int64.of_int (x * 8),
                 Value.eval_bop Bop.Bint_add sp_curr
                   (Value.of_num
                      (NumericValue.of_int64 (Int64.of_int (x * 8)) 8l))
                   8l )))
    in
    List.fold_left
      (fun acc (i, j) -> Result.bind acc (fun acc -> Frame.store_mem acc i j))
      (Frame.empty (fst bnd) (snd bnd) |> Result.ok)
      passing_vals

  let build_saved_sp (s : t) (p : Prog.t) (spdiff : Int64.t) :
      (Value.t, String.t) Result.t =
    let sp_curr = get_sp_curr s p in
    Value.eval_bop Bop.Bint_add sp_curr
      (Value.of_num (NumericValue.of_int64 spdiff 8l))
      8l

  let sp_extern (p : Prog.t) : RegId.t_full =
    { id = RegId.Register (Prog.sp_num p); offset = 0l; width = 8l }

  let add_sp_extern (s : t) (p : Prog.t) : (Value.t, String.t) Result.t =
    Value.eval_bop Bop.Bint_add
      (get_reg s (sp_extern p))
      (Value.of_num (NumericValue.of_int64 8L 8l))
      8l

  let get_regfile (s : t) : RegFile.t = s.regs
end
