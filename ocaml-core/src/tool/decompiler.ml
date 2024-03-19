open StdlibExt
open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "pcert -i <ifile>"
let ghidra_path = ref ""
let ifile = ref ""
let dump_path = ref ""
let func_path = ref ""

type dump_flag_type = {
  rom : bool ref;
  cfa : bool ref;
  l1 : bool ref;
  basic_block : bool ref;
  spfa : bool ref;
  l2 : bool ref;
  csa : bool ref;
  l3 : bool ref;
  rea : bool ref;
}

let dump_flag =
  {
    rom = ref true;
    cfa = ref false;
    l1 = ref false;
    basic_block = ref false;
    spfa = ref false;
    l2 = ref false;
    csa = ref false;
    l3 = ref false;
    rea = ref false;
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
    ("-dump-csa", Arg.Set dump_flag.csa, ": dump csa");
    ("-dump-rea", Arg.Set dump_flag.rea, ": dump rea");
    ("-dump-l3", Arg.Set dump_flag.l3, ": dump l3");
    ("-func-path", Arg.Set_string func_path, ": target funcs path");
    ("-project-cwd", Arg.Set_string cwd, ": set cwd");
    ("-debug", Arg.Unit (fun _ -> Logger.set_level Logger.Debug), ": debug mode");
    ("-log-path", Arg.String (fun x -> Logger.set_log_file x), ": log path");
    ( "-log-feature",
      Arg.String (fun x -> Logger.add_log_feature x),
      ": add log feature" );
  ]

let dump_cfa (cfa_res : (String.t * Addr.t * ILIR.Shallow_CFA.t) list)
    (dump_path : string) =
  List.iter
    (fun (fname, _, x) ->
      let dump_cfa_path = Filename.concat dump_path (fname ^ ".boundary") in
      let { ILIR.Shallow_CFA.sound_jump } = x in
      let contained_addrs =
        ILIR.JumpG.G.fold_vertex
          (fun l s -> LocSetD.add l s)
          sound_jump LocSetD.empty
      in
      let oc = open_out dump_cfa_path in
      let ofmt = Format.formatter_of_out_channel oc in
      let sorted_fboundary =
        LocSetD.to_seq contained_addrs
        |> Seq.filter (fun x -> snd x == 0)
        |> Seq.map (fun x -> fst x)
        |> List.of_seq |> List.sort compare
      in
      List.iter (fun x -> Format.fprintf ofmt "%Lx\n" x) sorted_fboundary;
      Format.fprintf ofmt "%!";
      close_out oc)
    cfa_res;
  ()

let dump_spfa (spfa_res : (FGIR.Func.t * FGIR.SPFA.Immutable.t) list)
    (dump_path : string) =
  List.iter
    (fun ((func : FGIR.Func.t), x) ->
      let fname : String.t =
        Option.value func.nameo
          ~default:(Format.asprintf "%a" Loc.pp func.entry)
      in
      let dump_spfa_path =
        Filename.concat dump_path (fname ^ ".stack_boundary")
      in
      let { FGIR.SPFA.Immutable.accesses } = x in
      let oc = open_out dump_spfa_path in
      let ofmt = Format.formatter_of_out_channel oc in
      match accesses with
      | FGIR.AccessD.Top -> Format.fprintf ofmt "Top\n"
      | FGIR.AccessD.Fin accesses ->
          (match Int64SetD.min_elt_opt accesses with
          | None -> Format.fprintf ofmt "Empty\n"
          | Some addr -> Format.fprintf ofmt "%Ld\n" addr);
          Format.fprintf ofmt "%!";
          close_out oc)
    spfa_res;
  ()

let main () =
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
        let cwd = if String.equal !cwd "" then [%pwd] else !cwd in
        let tmp_path = Filename.concat cwd "tmp" in
        if not (Sys.file_exists tmp_path) then Unix.mkdir tmp_path 0o777;
        [%log debug "Input file is %s" !ifile];
        let server = Ghidra.make_server !ifile !ghidra_path tmp_path cwd in
        List.iter (fun x -> [%log debug "func %s" x]) target_funcs;
        let func_with_addrs =
          List.map (fun x -> (x, Ghidra.get_func_addr server x)) target_funcs
        in
        let ifile_base =
          Filename.basename !ifile |> Filename.remove_extension
        in
        let _ = server.dump_rom (!dump_path ^ "/" ^ ifile_base ^ ".dmem") in

        let rom : DMem.t =
          Util.DMemWrapper.read_file (!dump_path ^ "/" ^ ifile_base ^ ".dmem")
        in
        let c0 = Sys.time () in
        let l0 : ILIR.Prog.t =
          {
            ins_mem = server.instfunc;
            rom;
            rspec = server.regspec.base_size;
            externs = Util.ExternalFunction.to_addrMap server.external_function;
          }
        in
        let c1 = Sys.time () in
        [%log info "L0 translation time: %f" (c1 -. c0)];
        let cfa_res : (String.t * Addr.t * ILIR.Shallow_CFA.t) list =
          func_with_addrs
          |> List.map (fun (fname, e) ->
                 (fname, e, ILIR.Shallow_CFA.follow_flow l0 e))
        in
        let c2 = Sys.time () in
        [%log info "CFA time: %f" (c2 -. c1)];
        let l1_init : FGIR_partial.Prog.t =
          FGIR_partial.L0toL1_shallow.translate_prog_from_cfa l0 cfa_res
        in
        let c3 = Sys.time () in
        [%log info "L1 translation time: %f" (c3 -. c2)];
        let l1_refine : FGIR_partial.Prog.t =
          FGIR_partial.Refine.apply_prog l0 l1_init
        in
        let c4 = Sys.time () in
        [%log info "L1 refinement time: %f" (c4 -. c3)];
        let l1 : FGIR.Prog.t = l1_refine |> FGIR.Prog.from_partial in
        let c5 = Sys.time () in
        [%log info "L1 time: %f" (c5 -. c4)];
        let cnt : int =
          l1.funcs
          |> List.fold_left
               (fun (a : Int64Set.t) (f : FGIR.Func.t) ->
                 List.fold_left
                   (fun (a : Int64Set.t) (b : FGIR.Block.t) ->
                     Int64Set.add_seq
                       (List.to_seq
                          (List.map
                             (fun (i : FGIR.Inst.t_full) -> fst i.loc)
                             b.body))
                       a)
                   a f.blocks)
               Int64Set.empty
          |> Int64Set.cardinal
        in
        [%log info "Total number of instructions: %d" cnt];
        let spfa_res : (FGIR.Func.t * FGIR.SPFA.Immutable.t) list =
          l1.funcs
          |> List.map (fun x -> (x, FGIR.SPFA.Immutable.analyze x 32l 40l))
        in
        let c6 = Sys.time () in
        [%log info "SPFA time: %f" (c6 -. c5)];
        let l2 : ASIR.Prog.t =
          ASIR.L1toL2.translate_prog_from_spfa l1 spfa_res 32l 40l
        in
        let c7 = Sys.time () in
        [%log info "L2 translation time: %f" (c7 -. c6)];
        let lva_res : (ASIR.Func.t * ASIR.REA.astate) List.t =
          ASIR.REA.compute_all l2
        in
        let c8 = Sys.time () in
        [%log info "REA time: %f" (c8 -. c7)];
        let l3 : IOIR.Prog.t = IOIR.L2toL3.translate_prog_from_rea l2 lva_res in
        let c9 = Sys.time () in
        [%log info "L3 translation time: %f" (c9 -. c8)];
        if
          (!(dump_flag.cfa) || !(dump_flag.l1) || !(dump_flag.spfa)
         || !(dump_flag.l2))
          && !dump_path = ""
        then
          [%log
            fatal
              "Dump path is not specified. Please specify dump path with \
               -dump-path"];
        if !(dump_flag.cfa) then dump_cfa cfa_res !dump_path else ();
        if !(dump_flag.l1) then (
          FGIR.Prog.write_prog l1 (!dump_path ^ "/" ^ ifile_base ^ ".fgir");
          Artifact.Dumper.dump (Artifact.Data.L1 l1)
            (!dump_path ^ "/" ^ ifile_base ^ ".fgir_dump"))
        else ();
        if !(dump_flag.basic_block) then
          FGIR.Prog.write_basic_block l1 !dump_path
        else ();
        if !(dump_flag.spfa) then dump_spfa spfa_res !dump_path else ();
        if !(dump_flag.l2) then (
          ASIR.Prog.write_prog l2 (!dump_path ^ "/" ^ ifile_base ^ ".asir");
          Artifact.Dumper.dump (Artifact.Data.L2 l2)
            (!dump_path ^ "/" ^ ifile_base ^ ".asir_dump"))
        else ();
        (* if !(dump_flag.csa) then
             dump_csa csa_res !dump_path
               (Filename.basename !ifile |> Filename.remove_extension)
           else (); *)
        if !(dump_flag.l3) then (
          IOIR.Prog.write_prog l3 (!dump_path ^ "/" ^ ifile_base ^ ".ioir");
          Artifact.Dumper.dump (Artifact.Data.L3 l3)
            (!dump_path ^ "/" ^ ifile_base ^ ".ioir_dump"))
        else ();
        (* if !(dump_flag.rea) then
             dump_rea lva_res !dump_path
               (Filename.basename !ifile |> Filename.remove_extension)
           else (); *)
        ()

let () = Global.run_main main
