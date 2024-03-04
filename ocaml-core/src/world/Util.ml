open StdlibExt
open Basic

let _ = Random.self_init ()

let create_server_socket ?(default_port = 0) () =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let port = default_port in
  let _ = Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) in
  let _ = Unix.listen fd 1 in
  let port =
    match Unix.getsockname fd with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> [%log fatal "impossible"]
  in
  (fd, port)

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

module RegSpec = struct
  module TMap = Map.Make (struct
    type t = Int32.t * Int32.t

    let compare (a1, a2) (b1, b2) =
      let c = Int32.compare a1 b1 in
      if c <> 0 then c else Int32.compare a2 b2
  end)

  type t = {
    base_size : Int32.t Int32Map.t;
    all_regs : (String.t * Int32.t * Int32.t) TMap.t;
  }

  let get (fd : Unix.file_descr) : t =
    [%log debug "RegSpec.get"];
    let num = Interaction.get_int fd in
    [%log debug "RegSpec.get: brs num=%ld" num];
    let rec loop acc n =
      if n = 0 then acc
      else (
        [%log debug "RegSpec.get: loop"];
        let offset = Interaction.get_int fd in
        let size = Interaction.get_int fd in
        [%log debug "RegSpec.get: baseid=%ld" offset];
        [%log debug "RegSpec.get: size=%ld" size];
        loop (Int32Map.add offset size acc) (n - 1))
    in
    let bs = loop Int32Map.empty (Int32.to_int num) in
    let num = Interaction.get_int fd in
    [%log debug "RegSpec.get: rs num=%ld" num];
    let rec loop acc n =
      if n = 0 then acc
      else (
        [%log debug "RegSpec.get: loop"];
        let name = Interaction.get_string fd in
        let baseid = Interaction.get_int fd in
        let offset = Interaction.get_int fd in
        let size = Interaction.get_int fd in
        [%log debug "RegSpec.get: name=%s" name];
        [%log debug "RegSpec.get: baseid=%ld" baseid];
        [%log debug "RegSpec.get: offset=%ld" offset];
        [%log debug "RegSpec.get: size=%ld" size];
        loop
          (TMap.add (Int32.add baseid offset, size) (name, baseid, offset) acc)
          (n - 1))
    in
    let rs = loop TMap.empty (Int32.to_int num) in
    { base_size = bs; all_regs = rs }
end

module ExternalFunction = struct
  type t = Int64.t List.t StringMap.t

  let get (fd : Unix.file_descr) : t =
    [%log debug "ExternalFunction.get"];
    let num = Interaction.get_int fd in
    let rec loop acc n =
      if n = 0 then acc
      else
        let name = Interaction.get_string fd in
        let num_args = Interaction.get_int fd in
        let args =
          List.init (Int32.to_int num_args) (fun _ -> Interaction.get_long fd)
        in
        loop (StringMap.add name args acc) (n - 1)
    in
    loop StringMap.empty (Int32.to_int num)

  let to_addrMap (ext : t) : String.t Basic_collection.AddrMap.t =
    StringMap.fold
      (fun name args acc ->
        List.fold_right
          (fun addr acc -> Basic_collection.AddrMap.add addr name acc)
          args acc)
      ext Basic_collection.AddrMap.empty
end

module DMemWrapper = struct
  type t = DMem.t

  let read (fd : Unix.file_descr) : t =
    let num = Interaction.read_int fd in
    let rec loop acc n =
      if n = 0 then acc
      else
        let addr = Interaction.read_long fd in
        let data = Interaction.read_string fd in
        loop (MemoryBlock.create addr data :: acc) (n - 1)
    in
    loop [] (Int32.to_int num) |> List.rev

  let read_file (path : String.t) : t =
    let ic = open_in_bin path in
    let res = read (Unix.descr_of_in_channel ic) in
    close_in ic;
    res
end
