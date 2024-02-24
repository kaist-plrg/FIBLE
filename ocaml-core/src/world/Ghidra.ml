open StdlibExt
open Basic
open Util
open Common_language

type t = {
  pid : int;
  fd : Unix.file_descr;
  instfunc : Int64.t -> (int * RawInst.t_full list) option;
  initstate : Int64.t -> Char.t;
  dump_rom : String.t -> String.t -> Int64.t;
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
      Global.devnull Global.devnull Global.devnull
  in
  [%log debug "Running ghidra at pid %d" ghidra_pid];
  Global.install_pid ghidra_pid;
  ghidra_pid

let varnode_raw_to_varnode (si : SpaceInfo.t) (rspec : RegSpec.t)
    (v : VarNode_Raw.t) : VarNode.t =
  if v.space = si.unique then
    Register
      { id = Unique (Int64.to_int32 v.offset); offset = 0l; width = v.size }
  else if v.space = si.register then
    let _, base, offset =
      RegSpec.TMap.find (Int64.to_int32 v.offset, v.size) rspec.all_regs
    in
    Register { id = Register base; offset; width = v.size }
  else if v.space = si.const then Const { value = v.offset; width = v.size }
  else if v.space = si.ram then Ram { value = v.offset; width = v.size }
  else [%log fatal "Unknown space %ld" v.space]

let pcode_raw_to_pcode (si : SpaceInfo.t) (rspec : RegSpec.t) (p : PCode_Raw.t)
    : RawInst.t_full =
  [%log debug "Converting %a" PCode_Raw.pp p];
  let inputs i = varnode_raw_to_varnode si rspec p.inputs.(i) in
  let output_raw () =
    varnode_raw_to_varnode si rspec
      ((p.output |> Option.map Fun.const
       |> Option.value ~default:(fun () ->
              [%log raise (Invalid_argument "option is None")]))
         ())
  in
  let output () =
    match output_raw () with
    | Register r -> r
    | _ -> [%log raise (Invalid_argument "Output is not a register")]
  in
  let mkJump _ =
    RawInst.Ijump
      (match inputs 0 with
      | Ram { value = a; _ } -> (a, 0)
      | _ -> [%log fatal "Jump target is not a constant"])
  in
  let mkJIump _ = RawInst.Ijump_ind (inputs 0) in
  let mkUop op =
    RawInst.Iassignment { expr = Auop (op, inputs 0); output = output () }
  in
  let mkBop op =
    RawInst.Iassignment
      { expr = Abop (op, inputs 0, inputs 1); output = output () }
  in
  let (inst : RawInst.t) =
    match p.opcode with
    | 0l -> Iunimplemented
    | 1l -> (
        match output_raw () with
        | Register r -> Iassignment { expr = Avar (inputs 0); output = r }
        | Ram { value = a; width = w } ->
            Istore
              {
                space = Const { value = Int64.of_int32 si.ram; width = 8l };
                pointer = Const { value = a; width = 8l };
                value = inputs 0;
              }
        | _ -> [%log fatal "Output is not a register or ram"])
    | 2l -> Iload { space = inputs 0; pointer = inputs 1; output = output () }
    | 3l -> Istore { space = inputs 0; pointer = inputs 1; value = inputs 2 }
    | 4l -> mkJump ()
    | 5l ->
        Icbranch
          {
            condition = inputs 1;
            target =
              (match inputs 0 with
              | Ram { value = a; _ } -> (a, 0)
              | _ -> [%log fatal "Jump target is not a constant"]);
          }
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
  [%log debug "Getting pcode list"];
  let num_pcodes = Interaction.get_int fd in
  [%log debug "Number of pcodes: %ld" num_pcodes];
  if num_pcodes = 0l then
    [ { mnemonic = "NOP"; opcode = 999l; inputs = [||]; output = None } ]
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
          List.map (pcode_raw_to_pcode spaceinfo regspec) (get_pcode_list fd)
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

  let dump_rom (fpath : String.t) (fname : String.t) =
    Interaction.put_char 'd';
    Interaction.put_string (fpath ^ "/" ^ fname ^ ".rom");
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
