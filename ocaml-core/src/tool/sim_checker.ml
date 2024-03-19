open StdlibExt
open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "sim_checker -i <ifile>"
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
    ("-log-path", Arg.String (fun x -> Logger.set_log_file x), ": log path");
    ( "-log-feature",
      Arg.String (fun x -> Logger.add_log_feature x),
      ": add log feature" );
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
        let _ =
          List.map (fun x -> (x, Ghidra.get_func_addr server x)) target_funcs
        in
        (*
                 let l0 : ILIR.Prog.t =
                   {
                     ins_mem = server.instfunc;
                     rom = server.initstate;
                     rspec = server.regspec.base_size;
                     externs = Util.ExternalFunction.to_addrMap server.external_function;
                   }
                 in
                 let cfa_res : (String.t * Addr.t * ILIR.Shallow_CFA.t) list =
                   func_with_addrs
                   |> List.map (fun (fname, e) ->
                          (fname, e, ILIR.Shallow_CFA.follow_flow l0 e))
                 in
                 let l1_init : FGIR_partial.Prog.t =
                   FGIR_partial.L0toL1_shallow.translate_prog_from_cfa l0 cfa_res
                 in
                 let l1_refine : FGIR_partial.Prog.t =
                   FGIR_partial.Refine.apply_prog l0 l1_init
                 in
                 let l1 : FGIR.Prog.t = l1_refine |> FGIR.Prog.from_partial in
                 let spfa_res : (FGIR.Func.t * FGIR.SPFA.Immutable.t) list =
                   FGIR.funcs |> List.map (fun x -> (x, FGIR.SPFA.Immutable.analyze x 32l))
                 in
                 let l2 : L2Partial.Prog.t =
                   L2Partial.L1toASIR.translate_prog_from_spfa l1 spfa_res 32l
                 in
                 (match
                    Simulation.Check_simulation.run server.regspec.base_size l0 l1 l2
                      (List.find (fun x -> fst x = "main") func_with_addrs |> snd)
                  with
                 | Ok _ -> Format.printf "Success\n"
                 | Error e -> Format.printf "Error: %s\n" e); *)
        Unix.kill server.pid Sys.sigterm

let () = Global.run_main main
