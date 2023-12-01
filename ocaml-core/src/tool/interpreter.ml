open StdlibExt
open Basic
open Basic_domain
open Value_domain
open World

let usage_msg = "interpreter -i <ifile>"
let ghidra_path = ref ""
let ifile = ref ""
let func_path = ref ""
let cwd = ref ""

let speclist =
  [
    ("-i", Arg.Set_string ifile, ": input file");
    ("-project-cwd", Arg.Set_string cwd, ": set cwd");
    ("-func-path", Arg.Set_string func_path, ": target funcs path");
    ("-debug", Arg.Unit (fun _ -> Logger.set_level Logger.Debug), ": debug mode");
  ]

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
        [%log debug "Input file is %s" !ifile];
        let server = Ghidra.make_server !ifile !ghidra_path tmp_path cwd in
        List.iter (fun x -> [%log debug "func %s" x]) target_funcs;
        let func_with_addrs =
          List.map (fun x -> (x, Ghidra.get_func_addr server x)) target_funcs
        in
        [%log
          debug "funcs: %a"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun fmt x ->
                 Format.fprintf fmt "(%s, %a)" (fst x) Addr.pp (snd x)))
            func_with_addrs];

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
          l1.funcs |> List.map (fun x -> (x, L1.SPFA.Immutable.analyze x 32L))
        in
        let l2 : L2.Prog.t =
          Translation.L1toL2.translate_prog_from_spfa l1 spfa_res 32L
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
        (match
           L4.Interp.interp l4
             (L4.Init.from_signature l4
                (List.find (fun x -> fst x = "main") func_with_addrs |> snd))
         with
        | Ok _ -> Format.printf "Success\n"
        | Error e -> Format.printf "Error: %s\n" e);
        Unix.kill server.pid Sys.sigterm

let () = Global.run_main main
