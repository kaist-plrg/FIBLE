open StdlibExt
open Basic

let _ = Random.self_init ()

let create_server_socket () =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let port = 0 in
  let _ = Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) in
  let _ = Unix.listen fd 1 in
  let port =
    match Unix.getsockname fd with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "impossible"
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
  type t = Int32.t StringMap.t

  let get (fd : Unix.file_descr) : t =
    Logger.debug "RegSpec.get\n";
    let num = Interaction.get_int fd in
    Logger.debug "RegSpec.get: num=%ld\n" num;
    let rec loop acc n =
      if n = 0 then acc
      else (
        Logger.debug "RegSpec.get: loop\n";
        let name = Interaction.get_string fd in
        let id = Interaction.get_int fd in
        Logger.debug "RegSpec.get %d: name=%s; id=%ld\n" n name id;
        loop (StringMap.add name id acc) (n - 1))
    in
    loop StringMap.empty (Int32.to_int num)
end

module ExternalFunction = struct
  type t = Int64.t List.t StringMap.t

  let get (fd : Unix.file_descr) : t =
    Logger.debug "ExternalFunction.get";
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
