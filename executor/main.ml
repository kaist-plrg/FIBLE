open Util;;

let usage_msg = "pcert -i <ifile> -g <ghidra_path> -p <ghidra_port>";;

let ghidra_path = ref "";;
let ifile = ref "";;

let speclist = [
  ("-i", Arg.String (fun x -> ifile := x), ": input file");
  ("-g", Arg.String (fun x -> ghidra_path := x), ": ghidra path");
];;

let create_server _ =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let port = 0 in
  let _ = Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) in
  let _ = Unix.listen fd 1 in
  let port = match Unix.getsockname fd with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "impossible" in
  (fd, port);;

let run_ghidra ifile tmp_path cwd port =
  let ghidra_headless_path = Filename.concat !ghidra_path "support/analyzeHeadless" in
  let ghidra_pid = Unix.create_process ghidra_headless_path [|ghidra_headless_path; tmp_path; "PcodeExtractor"; "-import"; ifile; "-postScript"; "PCert.java"; string_of_int port; "-scriptPath"; cwd; "-deleteProject" |] Unix.stdin Unix.stdout Unix.stderr in
    print_endline (Printf.sprintf "Running ghidra at pid %d" ghidra_pid);
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> Unix.kill ghidra_pid Sys.sigterm; exit 0));
  ghidra_pid;;

let instfunc (si: spaceinfo) (fd: Unix.file_descr) (addr: int64): (int * PCode.inst list) option =
    let iaddr = addr in
    print_endline (Printf.sprintf "Sending %Lx" (iaddr));
    Bytes.set sendbuf 0 'i';
    Bytes.set_int64_le sendbuf 1 iaddr;
    let _ = Unix.send fd sendbuf 0 9 [] in
    let inst_len = get_int fd in
    if inst_len = 0l then
      None
    else
      let pcodes = List.map (pcode_raw_to_pcode si) (get_pcode_list fd) in
      print_endline (Printf.sprintf "Received %d pcodes" (List.length pcodes));
      print_endline (String.concat "\n" (List.map string_of_pcode pcodes));
      Some (Int32.to_int inst_len, pcodes);;
  
let initstate (si: spaceinfo) (fd: Unix.file_descr) (addr: int64) =
    let iaddr = addr in
    Bytes.set sendbuf 0 's';
    Bytes.set_int64_le sendbuf 1 iaddr;
    let _ = Unix.send fd sendbuf 0 9 [] in
    0;;

let () =
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage_msg;
  if !ifile = "" then
    raise (Arg.Bad "No input file")
  else if !ghidra_path = "" then
    raise (Arg.Bad "No ghidra path")
  else
    let cwd = Sys.getcwd () in
    let tmp_path = Filename.concat cwd "tmp" in
    print_endline (Printf.sprintf "Input file is %s" !ifile);
    let (sfd, port) = create_server () in
    print_endline (Printf.sprintf "Listening on port %d" port);
    let ghidra_pid = run_ghidra !ifile tmp_path cwd port in
    
    let (fd, _) = Unix.accept sfd in
    print_endline (Printf.sprintf "Accepted connection");
    let main_addr_buf = Bytes.create 8 in
    print_endline (Printf.sprintf "Receiving main addr");
    let _ = Unix.recv fd main_addr_buf 0 8 [] in
    let main_addr = Bytes.get_int64_le main_addr_buf 0 in
    print_endline (Printf.sprintf "Main addr is %Lx" main_addr);
    let spaceinfo = get_stateinfo fd in
    print_endline (Printf.sprintf "Got stateinfo %ld %ld %ld" spaceinfo.unique spaceinfo.register spaceinfo.const);
    (*
    let a = run_loop {
      ins_mem = instfunc spaceinfo fd;
      entry_addr = int64ToWord64 main_addr
    } {
      registers = {
        ureg = (fun x -> PCode.wzero64);
        dreg = (fun x -> PCode.wzero64)
      };  
      memory = (initstate spaceinfo fd);
      pc = int64ToWord64 main_addr;
      seqnum = O
    } in
    print_endline (Printf.sprintf "PC after step %s" (Z.to_string (PCode.word64ToN a.pc))); *)
    let x = PCode.follow_flow {
      ins_mem = instfunc spaceinfo fd;
      entry_addr = main_addr
    } (main_addr) in
    let stop_addrs = (fst x) in
    let contained_addrs = (snd x) in
    print_endline (Printf.sprintf "Stop addrs %s" (String.concat ", " (List.map (fun x -> Printf.sprintf "%Lx" ((x))) stop_addrs)));
    print_endline (Printf.sprintf "Contained addrs %s" (String.concat ", " (List.map (fun x -> Printf.sprintf "%Lx" ((x))) contained_addrs)));
    Unix.sleep 4;
    Unix.kill ghidra_pid Sys.sigterm