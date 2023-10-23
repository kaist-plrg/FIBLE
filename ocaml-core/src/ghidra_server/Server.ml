open StdlibExt
open Basic
open Util

type t = {
  pid : int;
  fd : Unix.file_descr;
  instfunc : Int64.t -> (int * L0.Inst.t_full list) option;
  initstate : Int64.t -> Int64.t;
}

let create_server_socket _ =
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
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0o666 in
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
      |]
      devnull devnull devnull
  in
  Logger.debug "Running ghidra at pid %d\n" ghidra_pid;
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle
       (fun _ ->
         Unix.kill ghidra_pid Sys.sigterm;
         exit 0));
  ghidra_pid

let varnode_raw_to_varnode (si : SpaceInfo.t) (v : VarNode_Raw.t) : VarNode.t =
  if v.space = si.unique then Register { id = Unique v.offset; width = v.size }
  else if v.space = si.register then
    Register { id = Register v.offset; width = v.size }
  else if v.space = si.const then Const { value = v.offset; width = v.size }
  else if v.space = si.ram then Const { value = v.offset; width = v.size }
  else failwith (Format.sprintf "Unknown space %ld" v.space)

let pcode_raw_to_pcode (si : SpaceInfo.t) (p : PCode_Raw.t) : L0.Inst.t_full =
  Logger.debug "Converting %a\n" PCode_Raw.pp p;
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

let get_func_addr (server : t) (x : string) : int64 =
  Interaction.put_char 'f';
  Interaction.put_string x;
  Interaction.flush server.fd;
  Interaction.get_long server.fd

let get_pcode_list (fd : Unix.file_descr) : PCode_Raw.t list =
  Logger.debug "Getting pcode list\n";
  let num_pcodes = Interaction.get_int fd in
  Logger.debug "Number of pcodes: %ld\n" num_pcodes;
  if num_pcodes = 0l then
    [ { mnemonic = "NOP"; opcode = 999l; inputs = [||]; output = None } ]
  else
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (PCode_Raw.get fd :: acc) (n - 1)
    in
    List.rev (loop [] (Int32.to_int num_pcodes))

let tmpReg : RegId.t = { id = Unique 0L; width = 0l }

let make_server ifile ghidra_path tmp_path cwd : t =
  let sfd, port = create_server_socket () in
  Logger.debug "Listening on port %d\n" port;
  let ghidra_pid = run_ghidra ifile ghidra_path tmp_path cwd port in

  let x, _, _ = Unix.select [ sfd ] [] [] 30.0 in
  if x = [] then (
    Unix.kill ghidra_pid Sys.sigterm;
    failwith "No connection")
  else ();
  let fd, _ = Unix.accept sfd in
  Logger.debug "Accepted connection\n";
  let spaceinfo = SpaceInfo.get fd in
  Logger.debug "Got stateinfo %ld %ld %ld\n" spaceinfo.unique spaceinfo.register
    spaceinfo.const;
  let instHash : (int * L0.Inst.t_full list) Int64Hashtbl.t =
    Int64Hashtbl.create 1000
  in
  let instfunc (addr : int64) : (int * L0.Inst.t_full list) option =
    if Int64Hashtbl.mem instHash addr then
      Some (Int64Hashtbl.find instHash addr)
    else
      let iaddr = addr in
      Logger.debug "Sending %Lx\n" iaddr;
      Interaction.put_char 'i';
      Interaction.put_long iaddr;
      Interaction.flush fd;
      let inst_len = Interaction.get_int fd in
      if inst_len = 0l then None
      else
        let pcodes =
          List.map (pcode_raw_to_pcode spaceinfo) (get_pcode_list fd)
        in
        Logger.debug "Received %d pcodes\n" (List.length pcodes);
        List.iter (fun x -> Logger.debug "%a\n" L0.Inst.pp_full x) pcodes;
        Int64Hashtbl.add instHash addr (Int32.to_int inst_len, pcodes);
        Some (Int32.to_int inst_len, pcodes)
  in

  let initstate (addr : int64) =
    let iaddr = addr in
    Interaction.put_char 's';
    Interaction.put_long iaddr;
    Interaction.flush fd;
    Interaction.get_long fd
  in

  { pid = ghidra_pid; fd; instfunc; initstate }
