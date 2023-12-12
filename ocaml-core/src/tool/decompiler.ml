open StdlibExt
open Basic
open Basic_domain
open Value_domain
open World

let usage_msg = "pcert -i <ifile>"
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
  csa : bool ref;
  l3 : bool ref;
  rea : bool ref;
  l4 : bool ref;
}

let dump_flag =
  {
    cfa = ref false;
    l1 = ref false;
    basic_block = ref false;
    spfa = ref false;
    l2 = ref false;
    csa = ref false;
    l3 = ref false;
    rea = ref false;
    l4 = ref false;
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
    ("-dump-l3", Arg.Set dump_flag.l3, ": dump l3");
    ("-dump-rea", Arg.Set dump_flag.rea, ": dump rea");
    ("-dump-l4", Arg.Set dump_flag.l4, ": dump l4");
    ("-func-path", Arg.Set_string func_path, ": target funcs path");
    ("-project-cwd", Arg.Set_string cwd, ": set cwd");
    ("-debug", Arg.Unit (fun _ -> Logger.set_level Logger.Debug), ": debug mode");
    ("-log-path", Arg.String (fun x -> Logger.set_log_file x), ": log path");
    ( "-log-feature",
      Arg.String (fun x -> Logger.add_log_feature x),
      ": add log feature" );
  ]

let dump_cfa (cfa_res : (String.t * Addr.t * L0.Shallow_CFA.t) list)
    (dump_path : string) =
  List.iter
    (fun (fname, _, x) ->
      let dump_cfa_path = Filename.concat dump_path (fname ^ ".boundary") in
      let { L0.Shallow_CFA.sound_jump } = x in
      let contained_addrs =
        L0.JumpG.G.fold_vertex
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

        let l0 : L0.Prog.t =
          {
            ins_mem = server.instfunc;
            rom = server.initstate;
            externs = Util.ExternalFunction.to_addrMap server.external_function;
          }
        in
        let cfa_res : (String.t * Addr.t * L0.Shallow_CFA.t) list =
          func_with_addrs
          |> List.map (fun (fname, e) ->
                 (fname, e, L0.Shallow_CFA.follow_flow l0 e))
        in
        let l1_init : L1Partial.Prog.t =
          L1Partial.L0toL1_shallow.translate_prog_from_cfa l0 cfa_res
        in
        let l1_refine : L1Partial.Prog.t =
          L1Partial.Refine.apply_prog l0 l1_init
        in
        let l1 : L1.Prog.t = l1_refine |> L1.Prog.from_partial in
        let spfa_res : (L1.Func.t * L1.SPFA.Immutable.t) list =
          l1.funcs |> List.map (fun x -> (x, L1.SPFA.Immutable.analyze x 32l))
        in
        let l2 : L2.Prog.t =
          Translation.L1toL2.translate_prog_from_spfa l1 spfa_res 32l
        in
        let csa_res : (L2.Func.t * L2.CSA.Immutable.t) list =
          l2.funcs |> List.map (fun x -> (x, L2.CSA.Immutable.analyze x))
        in
        let l3 : L3.Prog.t =
          Translation.L2toL3.translate_prog_from_csa l2 csa_res
        in
        let lva_res : (L3.Func.t * L3.REA.astate) List.t =
          L3.REA.compute_all l3
        in
        let l4 : L4.Prog.t =
          Translation.L3toL4.translate_prog_from_rea l3 lva_res
        in
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
          L1.Prog.dump_prog l1 !dump_path
            (Filename.basename !ifile |> Filename.remove_extension);
          L1Partial.Prog.dump_prog l1_refine !dump_path
            (Filename.basename !ifile |> Filename.remove_extension))
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
        (* if !(dump_flag.csa) then
             dump_csa csa_res !dump_path
               (Filename.basename !ifile |> Filename.remove_extension)
           else (); *)
        if !(dump_flag.l3) then
          L3.Prog.dump_prog l3 !dump_path
            (Filename.basename !ifile |> Filename.remove_extension)
        else ();
        (* if !(dump_flag.rea) then
             dump_rea lva_res !dump_path
               (Filename.basename !ifile |> Filename.remove_extension)
           else (); *)
        if !(dump_flag.l4) then
          L4.Prog.dump_prog l4 !dump_path
            (Filename.basename !ifile |> Filename.remove_extension)
        else ();
        ()

let () = Global.run_main main
