open StdlibExt
open Basic

let print_endline = Interaction.print_endline

module SpaceInfo = struct
  type t = { unique : int32; register : int32; const : int32; ram : int32 }

  let pp fmt (s : t) =
    Format.fprintf fmt "{unique=%ld; register=%ld; const=%ld}" s.unique
      s.register s.const

  let get (fd : Unix.file_descr) : t =
    let unique = Interaction.get_int fd in
    let register = Interaction.get_int fd in
    let const = Interaction.get_int fd in
    let ram = Interaction.get_int fd in
    { unique; register; const; ram }
end

module VarNode_Raw = struct
  type t = { space : int32; offset : int64; size : int32 }

  let pp fmt (v : t) =
    Format.fprintf fmt "{space=%ld; offset=%Ld; size=%ld}" v.space v.offset
      v.size

  let get (fd : Unix.file_descr) : t =
    let space = Interaction.get_int fd in
    let offset = Interaction.get_long fd in
    let size = Interaction.get_int fd in
    { space; offset; size }
end

module PCode_Raw = struct
  type t = {
    opcode : int32;
    mnemonic : string;
    inputs : VarNode_Raw.t array;
    output : VarNode_Raw.t option;
  }

  let pp fmt (p : t) =
    Format.fprintf fmt "{mnemonic: %s; opcode=%ld; inputs=%a; output=%a}"
      p.mnemonic p.opcode
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         VarNode_Raw.pp)
      (Array.to_list p.inputs)
      (fun fmt -> function
        | None -> Format.fprintf fmt "None"
        | Some v -> Format.fprintf fmt "%a" VarNode_Raw.pp v)
      p.output

  let get (fd : Unix.file_descr) : t =
    let mnemonic = Interaction.get_string fd in
    let opcode = Interaction.get_int fd in
    let num_inputs = Interaction.get_int fd in
    let inputs =
      Array.init (Int32.to_int num_inputs) (fun _ -> VarNode_Raw.get fd)
    in
    let exists_output = Interaction.get_int fd in
    let output =
      if exists_output = 0l then None else Some (VarNode_Raw.get fd)
    in
    { mnemonic; opcode; inputs; output }
end

let get_func_addr (fd : Unix.file_descr) (x : string) : int64 =
  Interaction.put_char 'f';
  Interaction.put_string x;
  Interaction.flush fd;
  Interaction.get_long fd

let get_pcode_list (fd : Unix.file_descr) : PCode_Raw.t list =
  print_endline "Getting pcode list";
  let num_pcodes = Interaction.get_int fd in
  print_endline (Format.sprintf "Number of pcodes: %ld" num_pcodes);
  if num_pcodes = 0l then
    [ { mnemonic = "NOP"; opcode = 999l; inputs = [||]; output = None } ]
  else
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (PCode_Raw.get fd :: acc) (n - 1)
    in
    List.rev (loop [] (Int32.to_int num_pcodes))

let tmpReg : RegId.t = { id = Unique 0L; width = 0l }

let varnode_raw_to_varnode (si : SpaceInfo.t) (v : VarNode_Raw.t) : VarNode.t =
  if v.space = si.unique then Register { id = Unique v.offset; width = v.size }
  else if v.space = si.register then
    Register { id = Register v.offset; width = v.size }
  else if v.space = si.const then Const { value = v.offset; width = v.size }
  else if v.space = si.ram then Const { value = v.offset; width = v.size }
  else failwith (Format.sprintf "Unknown space %ld" v.space)

let pcode_raw_to_pcode (si : SpaceInfo.t) (p : PCode_Raw.t) : L0.Inst.t_full =
  print_endline (Format.asprintf "Converting %a" PCode_Raw.pp p);
  let inputs i = varnode_raw_to_varnode si p.inputs.(i) in
  let output _ =
    match varnode_raw_to_varnode si (Option.get p.output) with
    | Register r -> r
    | _ -> raise (Invalid_argument "Output is not a register")
  in
  let mkJump _ =
    L0.Inst.Ijump
      (match inputs 0 with
      | Const { value = a; _ } -> (a, 0)
      | _ -> failwith "Jump target is not a constant")
  in
  let mkJIump _ = L0.Inst.Ijump_ind (inputs 0) in
  let mkUop op = L0.Inst.Iassignment (Auop (op, inputs 0), output ()) in
  let mkBop op =
    L0.Inst.Iassignment (Abop (op, inputs 0, inputs 1), output ())
  in
  let (inst : L0.Inst.t) =
    match p.opcode with
    | 0l -> Iunimplemented
    | 1l -> Iassignment (Avar (inputs 0), output ())
    | 2l -> Iload (inputs 0, inputs 1, output ())
    | 3l -> Istore (inputs 0, inputs 1, inputs 2)
    | 4l -> mkJump ()
    | 5l ->
        Icbranch
          ( inputs 1,
            match inputs 0 with
            | Const { value = a; _ } -> (a, 0)
            | _ -> failwith "Jump target is not a constant" )
    | 6l -> mkJIump ()
    | 7l -> mkJump ()
    | 8l -> mkJIump ()
    | 9l -> Iunimplemented
    | 10l -> mkJIump ()
    | 11l -> mkBop Bint_equal
    | 12l -> mkBop Bint_notequal
    | 13l -> mkBop Bint_sless
    | 14l -> mkBop Bint_slessequal
    | 15l -> mkBop Bint_less
    | 16l -> mkBop Bint_lessequal
    | 17l -> mkUop Uint_zext
    | 18l -> mkUop Uint_sext
    | 19l -> mkBop Bint_add
    | 20l -> mkBop Bint_sub
    | 21l -> mkBop Bint_carry
    | 22l -> mkBop Bint_scarry
    | 23l -> mkBop Bint_sborrow
    | 24l -> mkUop Uint_2comp
    | 25l -> mkUop Uint_negate
    | 26l -> mkBop Bint_xor
    | 27l -> mkBop Bint_and
    | 28l -> mkBop Bint_or
    | 29l -> mkBop Bint_left
    | 30l -> mkBop Bint_right
    | 31l -> mkBop Bint_sright
    | 32l -> mkBop Bint_mult
    | 33l -> mkBop Bint_div
    | 34l -> mkBop Bint_sdiv
    | 35l -> mkBop Bint_rem
    | 36l -> mkBop Bint_srem
    | 37l -> mkUop Ubool_negate
    | 38l -> mkBop Bbool_xor
    | 39l -> mkBop Bbool_and
    | 40l -> mkBop Bbool_or
    | 41l -> mkBop Bfloat_equal
    | 42l -> mkBop Bfloat_notequal
    | 43l -> mkBop Bfloat_less
    | 44l -> mkBop Bfloat_lessequal
    | 46l -> mkUop Ufloat_nan
    | 47l -> mkBop Bfloat_add
    | 48l -> mkBop Bfloat_div
    | 49l -> mkBop Bfloat_mult
    | 50l -> mkBop Bfloat_sub
    | 51l -> mkUop Ufloat_neg
    | 52l -> mkUop Ufloat_abs
    | 53l -> mkUop Ufloat_sqrt
    | 54l -> mkUop Uint2float
    | 55l -> mkUop Ufloat2float
    | 57l -> mkUop Ufloat_ceil
    | 58l -> mkUop Ufloat_floor
    | 59l -> mkUop Ufloat_round
    | 62l -> mkBop Bpiece
    | 63l -> mkBop Bsubpiece
    | 72l -> mkUop Upopcount
    | 73l -> mkUop Ulzcount
    | 999l -> INop
    | _ -> Iunimplemented
  in
  { ins = inst; mnem = p.mnemonic }
