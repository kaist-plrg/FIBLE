open StdlibExt
open Basic
open Basic_domain
open Value_domain
open Ghidra_server

let usage_msg = "pcert -i <ifile> -g <ghidra_path> -p <ghidra_port>"
let ghidra_path = ref ""
let ifile = ref ""
let dump_path = ref ""
let func_path = ref ""

type dump_flag_type = {
  cfa : bool ref;
  l1 : bool ref;
  basic_block : bool ref;
  spfa : bool ref;
  l2 : bool ref;
  run : bool ref;
}

let dump_flag =
  {
    cfa = ref false;
    l1 = ref false;
    basic_block = ref false;
    spfa = ref false;
    l2 = ref false;
    run = ref false;
  }

let cwd = ref ""

let speclist =
  [
    ("-i", Arg.Set_string ifile, ": input file");
    ("-dump-path", Arg.Set_string dump_path, ": dump path");
    ("-dump-cfa", Arg.Set dump_flag.cfa, ": dump cfa");
    ("-dump-l1", Arg.Set dump_flag.l1, ": dump l1");
    ("-dump-basic-block", Arg.Set dump_flag.basic_block, ": dump basic block");
    ("-dump-spfa", Arg.Set dump_flag.spfa, ": dump spfa");
    ("-dump-l2", Arg.Set dump_flag.l2, ": dump l2");
    ("-func-path", Arg.Set_string func_path, ": target funcs path");
    ("-run-sim", Arg.Set dump_flag.run, ": run simulator and dump result");
    ("-project-cwd", Arg.Set_string cwd, ": set cwd");
    ("-debug", Arg.Unit (fun _ -> Logger.set_level Logger.Debug), ": debug mode");
  ]

let dump_cfa (cfa_res : (String.t * Addr.t * L0.CFA.Immutable.t) list)
    (dump_path : string) =
  List.iter
    (fun (fname, _, x) ->
      let dump_cfa_path = Filename.concat dump_path (fname ^ ".boundary") in
      let { L0.CFA.Immutable.abs_state } = x in
      let contained_addrs = abs_state.pre_state in
      let oc = open_out dump_cfa_path in
      let ofmt = Format.formatter_of_out_channel oc in
      let sorted_fboundary =
        L0.FSAbsD.AbsLocMapD.to_seq contained_addrs
        |> Seq.filter (fun x -> snd (fst x) == 0)
        |> Seq.map (fun x -> fst (fst x))
        |> List.of_seq |> List.sort compare
      in
      List.iter (fun x -> Format.fprintf ofmt "%Lx\n" x) sorted_fboundary;
      Format.fprintf ofmt "%!";
      close_out oc)
    cfa_res;
  ()

let dump_spfa (spfa_res : (L1.Func.t * L1.SPFA.Immutable.t) list)
    (dump_path : string) =
  List.iter
    (fun ((func : L1.Func.t), x) ->
      let fname : String.t =
        Option.value func.nameo
          ~default:(Format.asprintf "%a" Loc.pp func.entry)
      in
      let dump_spfa_path =
        Filename.concat dump_path (fname ^ ".stack_boundary")
      in
      let { L1.SPFA.Immutable.accesses } = x in
      let oc = open_out dump_spfa_path in
      let ofmt = Format.formatter_of_out_channel oc in
      match accesses with
      | L1.AccessD.Top -> Format.fprintf ofmt "Top\n"
      | L1.AccessD.Fin accesses ->
          (match Int64SetD.min_elt_opt accesses with
          | None -> Format.fprintf ofmt "Empty\n"
          | Some addr -> Format.fprintf ofmt "%Ld\n" addr);
          Format.fprintf ofmt "%!";
          close_out oc)
    spfa_res;
  ()

let () =
  match Sys.getenv_opt "GHIDRA_PATH" with
  | None ->
      raise
        (Arg.Bad "No ghidra path, please set GHIDRA_PATH environment variable")
  | Some x ->
      ghidra_path := x;
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
        let cwd = if String.equal !cwd "" then Sys.getcwd () else !cwd in
        let tmp_path = Filename.concat cwd "tmp" in
        Logger.debug "Input file is %s\n" !ifile;
        let server = Server.make_server !ifile !ghidra_path tmp_path cwd in
        List.iter (fun x -> Logger.debug "func %s\n" x) target_funcs;
        let func_with_addrs =
          List.map (fun x -> (x, Server.get_func_addr server x)) target_funcs
        in

        let l0 : L0.Prog.t =
          { ins_mem = server.instfunc; rom = server.initstate }
        in
        let cfa_res : (String.t * Addr.t * L0.CFA.Immutable.t) list =
          func_with_addrs
          |> List.map (fun (fname, e) ->
                 (fname, e, L0.CFA.Immutable.follow_flow l0 e))
        in
        let l1 : L1.Prog.t =
          Translation.L0toL1.translate_prog_from_cfa l0 cfa_res
        in
        let spfa_res : (L1.Func.t * L1.SPFA.Immutable.t) list =
          l1.funcs |> List.map (fun x -> (x, L1.SPFA.Immutable.analyze x 32L))
        in
        let l2 : L2.Prog.t =
          Translation.L1toL2.translate_prog_from_spfa l1 spfa_res 32L
        in
        if
          (!(dump_flag.cfa) || !(dump_flag.l1) || !(dump_flag.spfa)
         || !(dump_flag.l2))
          && !dump_path = ""
        then
          failwith
            "Dump path is not specified. Please specify dump path with \
             -dump-path";
        if !(dump_flag.cfa) then dump_cfa cfa_res !dump_path else ();
        if !(dump_flag.l1) then
          L1.Prog.dump_prog l1 !dump_path
            (Filename.basename !ifile |> Filename.remove_extension)
        else ();
        if !(dump_flag.basic_block) then
          L1.Prog.dump_basic_block l1 !dump_path
            (Filename.basename !ifile |> Filename.remove_extension)
        else ();
        if !(dump_flag.spfa) then dump_spfa spfa_res !dump_path else ();
        if !(dump_flag.l2) then
          L2.Prog.dump_prog l2 !dump_path
            (Filename.basename !ifile |> Filename.remove_extension)
        else ();
        if !(dump_flag.run) then
          match
            Simulation.Check_simulation.run l0 l1 l2
              (List.find (fun x -> fst x = "main") func_with_addrs |> snd)
          with
          | Ok _ -> Logger.info "Success\n"
          | Error e -> Logger.info "Error: %s\n" e
        else ();
        Unix.kill server.pid Sys.sigterm
