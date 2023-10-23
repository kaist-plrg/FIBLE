open StdlibExt
open Basic

let _ = Random.self_init ()

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
