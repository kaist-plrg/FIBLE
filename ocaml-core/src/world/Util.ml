open Common

let _ = Random.self_init ()

let create_server_socket ?(default_port = 0) () =
  let fd = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd SO_REUSEADDR true;
  let port = default_port in
  let _ = Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) in
  let _ = Unix.listen fd 1 in
  let port =
    match Unix.getsockname fd with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> [%log fatal "impossible"]
  in
  (fd, port)

let rec accept_non_intr s =
  try Unix.accept ~cloexec:true s
  with Unix.Unix_error (EINTR, _, _) -> accept_non_intr s

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (EINTR, _, _) -> waitpid_non_intr pid

let establish_server server_fun sock =
  Unix.listen sock 5;
  let s, _caller = accept_non_intr sock in
  (* The "double fork" trick, the process which calls server_fun will not
     leave a zombie process *)
  match Unix.fork () with
  | 0 ->
      if Unix.fork () <> 0 then Unix._exit 0;
      (* The child exits, the grandchild works *)
      Unix.close sock;
      let inchan = Unix.in_channel_of_descr s in
      let outchan = Unix.out_channel_of_descr s in
      server_fun inchan outchan;
      (* Do not close inchan nor outchan, as the server_fun could
         have done it already, and we are about to exit anyway
         (PR#3794) *)
      exit 0
  | id ->
      Unix.close s;
      ignore (waitpid_non_intr id)
(* Reclaim the child *)

let calc_stack_size (args : String.t List.t) (env : String.t List.t) : Int64.t =
  let args_size =
    List.fold_left
      (fun acc x ->
        let len = Int64.of_int (String.length x) in
        let padded_len = Int64.mul (Int64.div (Int64.add len 8L) 8L) 8L in
        Int64.add acc padded_len)
      0L args
  in
  let env_size =
    List.fold_left
      (fun acc x ->
        let len = Int64.of_int (String.length x) in
        let padded_len = Int64.mul (Int64.div (Int64.add len 8L) 8L) 8L in
        Int64.add acc padded_len)
      0L env
  in
  let args_ptr_size = Int64.of_int (List.length args + 1) |> Int64.mul 8L in
  let env_ptr_size = Int64.of_int (List.length env + 1) |> Int64.mul 8L in
  Int64.add
    (Int64.add args_size env_size)
    (Int64.add args_ptr_size env_ptr_size)
  |> Int64.add 64L

external getfl : int -> int = "unix_getfl"
external fd_is_valid : int -> bool = "unix_fd_is_valid"
external open_ : string -> int -> int -> int = "unix_open"
external close : int -> int = "unix_close"
external read : int -> bytes -> Int64.t -> Int64.t = "unix_read"
external write : int -> string -> Int64.t -> Int64.t = "unix_write"
external stat : string -> bytes -> int = "unix_stat"
external fstat : int -> bytes -> int = "unix_fstat"
external lstat : string -> bytes -> int = "unix_lstat"
external ioctl : int -> int -> Int64.t -> int = "unix_ioctl"
external getcwd : bytes -> int -> int = "unix_getcwd"
external chroot : string -> int = "unix_chroot"
external fadvise : int -> Int64.t -> Int64.t -> int -> int = "unix_fadvise"
external openat : int -> string -> int -> int -> int = "unix_openat"

module SpaceInfo = struct
  type t = SleighDef.SpaceInfo.t

  let get (fd : Unix.file_descr) : t =
    let unique = Interaction.get_int fd in
    let register = Interaction.get_int fd in
    let const = Interaction.get_int fd in
    let ram = Interaction.get_int fd in
    { SleighDef.SpaceInfo.unique; register; const; ram }
end

module VarNode_Raw = struct
  type t = SleighDef.VarNode.t

  let get (fd : Unix.file_descr) : t =
    let space = Interaction.get_int fd in
    let offset = Interaction.get_long fd in
    let size = Interaction.get_int fd in
    { SleighDef.VarNode.space; offset; size }
end

module PCode_Raw = struct
  type t = SleighDef.PCode.t

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
  type t = SleighDef.RegSpec.t

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
          (SleighDef.RegSpec.TMap.add
             (Int32.add baseid offset, size)
             (name, baseid, offset) acc)
          (n - 1))
    in
    let rs = loop SleighDef.RegSpec.TMap.empty (Int32.to_int num) in
    { SleighDef.RegSpec.base_size = bs; all_regs = rs }
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

  let to_addrMap (ext : t) : String.t Byte8Map.t =
    StringMap.fold
      (fun name args acc ->
        List.fold_right (fun addr acc -> Byte8Map.add addr name acc) args acc)
      ext Byte8Map.empty
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
