open StdlibExt
open Basic
open Basic_domain
open Value_domain
open State_domain
open Util;;

Random.self_init ()

let usage_msg = "pcert -i <ifile> -g <ghidra_path> -p <ghidra_port>"
let ghidra_path = ref ""
let ifile = ref ""
let dump_cfa_path = ref ""
let func_path = ref ""

let speclist =
  [
    ("-i", Arg.String (fun x -> ifile := x), ": input file");
    ("-g", Arg.String (fun x -> ghidra_path := x), ": ghidra path");
    ( "-dump-cfa-path",
      Arg.String (fun x -> dump_cfa_path := x),
      ": dump cfa path" );
    ("-func-path", Arg.String (fun x -> func_path := x), ": target funcs path");
  ]

let create_server _ =
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

let run_ghidra ifile tmp_path cwd port =
  let ghidra_headless_path =
    Filename.concat !ghidra_path "support/analyzeHeadless"
  in
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
      Unix.stdin Unix.stdout Unix.stderr
  in
  print_endline (Printf.sprintf "Running ghidra at pid %d" ghidra_pid);
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle
       (fun _ ->
         Unix.kill ghidra_pid Sys.sigterm;
         exit 0));
  ghidra_pid

let instHash : (int * Inst.t list) Int64Hashtbl.t = Int64Hashtbl.create 1000

let instfunc (si : spaceinfo) (fd : Unix.file_descr) (addr : int64) :
    (int * Inst.t list) option =
  if Int64Hashtbl.mem instHash addr then Some (Int64Hashtbl.find instHash addr)
  else
    let iaddr = addr in
    print_endline (Printf.sprintf "Sending %Lx" iaddr);
    put_char 'i';
    put_long iaddr;
    flush fd;
    let inst_len = get_int fd in
    if inst_len = 0l then None
    else
      let pcodes = List.map (pcode_raw_to_pcode si) (get_pcode_list fd) in
      print_endline (Printf.sprintf "Received %d pcodes" (List.length pcodes));
      List.iter (fun x -> Format.printf "%a\n" Inst.pp x) pcodes;
      Int64Hashtbl.add instHash addr (Int32.to_int inst_len, pcodes);
      Some (Int32.to_int inst_len, pcodes)

let initstate (si : spaceinfo) (fd : Unix.file_descr) (addr : int64) =
  let iaddr = addr in
  put_char 's';
  put_long iaddr;
  flush fd;
  get_long fd

let () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage_msg;
  if !ifile = "" then raise (Arg.Bad "No input file")
  else if !ghidra_path = "" then raise (Arg.Bad "No ghidra path")
  else
    let target_funcs =
      if !func_path <> "" then (
        let ic = open_in !func_path in
        let rec read_lines acc =
          try
            let line = input_line ic in
            if line <> "" then read_lines (line :: acc) else read_lines acc
          with End_of_file -> acc
        in
        let lines = read_lines [] in
        close_in ic;
        lines)
      else [ "main" ]
    in
    let cwd = Sys.getcwd () in
    let tmp_path = Filename.concat cwd "tmp" in
    print_endline (Printf.sprintf "Input file is %s" !ifile);
    let sfd, port = create_server () in
    print_endline (Printf.sprintf "Listening on port %d" port);
    let ghidra_pid = run_ghidra !ifile tmp_path cwd port in

    let fd, _ = Unix.accept sfd in
    print_endline (Printf.sprintf "Accepted connection");
    let spaceinfo = get_stateinfo fd in
    print_endline
      (Printf.sprintf "Got stateinfo %ld %ld %ld" spaceinfo.unique
         spaceinfo.register spaceinfo.const);
    List.iter (fun x -> print_endline (Printf.sprintf "func %s" x)) target_funcs;
    let func_with_addrs =
      List.map (fun x -> (x, get_func_addr fd x)) target_funcs
    in

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
    List.iter
      (fun (fname, y) ->
        let x =
          CFA.Mutable.follow_flow
            {
              ins_mem = instfunc spaceinfo fd;
              rom = initstate spaceinfo fd;
              entry_addr = y;
            }
            y
        in
        let stop_addrs = snd x.analysis_contour.boundary_point in
        let contained_addrs = x.abs_state.pre_state in
        print_endline
          (Printf.sprintf "Stop addrs %s"
             (String.concat ", "
                (List.map
                   (fun x -> Printf.sprintf "%Lx" (fst x))
                   (List.of_seq (LocSetD_Mut.to_seq stop_addrs)))));
        print_endline
          (Printf.sprintf "Contained addrs %s"
             (String.concat ", "
                (List.map
                   (fun x -> Printf.sprintf "%Lx" (fst (fst x)))
                   (FSAbsD_Mut.AbsLocMapD.to_seq contained_addrs
                   |> Seq.filter (fun x -> snd (fst x) == 0)
                   |> List.of_seq))));
        if !dump_cfa_path <> "" then (
          let dump_cfa_path =
            Filename.concat !dump_cfa_path (fname ^ ".boundary")
          in
          let oc = open_out dump_cfa_path in
          let sorted_fboundary =
            FSAbsD_Mut.AbsLocMapD.to_seq contained_addrs
            |> Seq.filter (fun x -> snd (fst x) == 0)
            |> Seq.map (fun x -> fst (fst x))
            |> List.of_seq |> List.sort compare
          in
          List.iter (fun x -> Printf.fprintf oc "%Lx\n" x) sorted_fboundary;
          close_out oc))
      func_with_addrs;
    Unix.sleep 4;
    Unix.kill ghidra_pid Sys.sigterm
