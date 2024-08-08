open Util
open Common

type t = {
  pid : int;
  fd : Unix.file_descr;
  instfunc : Int64.t -> (int * RawInst.t_full list) option;
  initstate : Int64.t -> Char.t;
  dump_rom : String.t -> Int64.t;
  external_function : ExternalFunction.t;
  regspec : RegSpec.t;
}

let gen_random_name (fname : string) =
  let rand = Random.int 1000000000 in
  let fname = Filename.basename fname in
  let fname = Filename.remove_extension fname in
  let fname = "PEx" ^ fname ^ string_of_int rand in
  fname

let run_ghidra ifile ghidra_path tmp_path cwd port =
  let ghidra_headless_path =
    Filename.concat ghidra_path "support/analyzeHeadless"
  in
  (* ignore stdin/stdout *)
  let ghidra_pid =
    Unix.create_process ghidra_headless_path
      [|
        ghidra_headless_path;
        tmp_path;
        gen_random_name ifile;
        "-import";
        ifile;
        "-postScript";
        "PCert.java";
        string_of_int port;
        "-scriptPath";
        cwd;
        "-deleteProject";
        "-noanalysis";
      |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  [%log debug "Running ghidra at pid %d" ghidra_pid];
  Global.install_pid ghidra_pid;
  ghidra_pid

let get_func_addr (server : t) (x : string) : int64 =
  Interaction.put_char 'f';
  Interaction.put_string x;
  Interaction.flush server.fd;
  Interaction.get_long server.fd

let get_pcode_list (fd : Unix.file_descr) : PCode_Raw.t list =
  [%log debug "Getting pcode list"];
  let num_pcodes = Interaction.get_int fd in
  [%log debug "Number of pcodes: %ld" num_pcodes];
  if num_pcodes = 0l then
    [ { mnemonic = "NOP:0"; opcode = 999l; inputs = [||]; output = None } ]
  else
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (PCode_Raw.get fd :: acc) (n - 1)
    in
    List.rev (loop [] (Int32.to_int num_pcodes))

let tmpReg : RegId.t_full = { id = Unique 0l; offset = 0l; width = 0l }

let make_server ifile ghidra_path tmp_path cwd : t =
  let sfd, port = Util.create_server_socket () in
  [%log debug "Listening on port %d" port];
  let ghidra_pid = run_ghidra ifile ghidra_path tmp_path cwd port in

  let x, _, _ = Unix.select [ sfd ] [] [] 60.0 in
  if x = [] then [%log fatal "No connection"] else ();
  let fd, _ = Unix.accept sfd in
  [%log debug "Accepted connection"];
  let spaceinfo = SpaceInfo.get fd in
  let regspec = RegSpec.get fd in
  let external_function = ExternalFunction.get fd in
  [%log
    debug "Got stateinfo %ld %ld %ld" spaceinfo.unique spaceinfo.register
      spaceinfo.const];
  let instHash : (int * RawInst.t_full list) Int64Hashtbl.t =
    Int64Hashtbl.create 1000
  in
  let instfunc (addr : int64) : (int * RawInst.t_full list) option =
    if Int64Hashtbl.mem instHash addr then
      Some (Int64Hashtbl.find instHash addr)
    else
      let iaddr = addr in
      [%log debug "Sending %Lx" iaddr];
      Interaction.put_char 'i';
      Interaction.put_long iaddr;
      Interaction.flush fd;
      let inst_len = Interaction.get_int fd in
      if inst_len = 0l then None
      else
        let pcodes =
          List.mapi
            (SleighDef.Translate.pcode_to_common spaceinfo regspec addr)
            (get_pcode_list fd)
        in
        [%log debug "Received %d pcodes" (List.length pcodes)];
        List.iter (fun x -> [%log debug "%a\n" RawInst.pp_full x]) pcodes;
        Int64Hashtbl.add instHash addr (Int32.to_int inst_len, pcodes);
        Some (Int32.to_int inst_len, pcodes)
  in

  let initstate (addr : int64) =
    let iaddr = addr in
    Interaction.put_char 's';
    Interaction.put_long iaddr;
    Interaction.flush fd;
    Interaction.get_char fd
  in

  let dump_rom (fpath : String.t) =
    Interaction.put_char 'd';
    Interaction.put_string fpath;
    Interaction.flush fd;
    Interaction.get_long fd
  in

  {
    pid = ghidra_pid;
    fd;
    instfunc;
    initstate;
    dump_rom;
    external_function;
    regspec;
  }
