open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "sim_checker -i <ifile>"
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
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage_msg;
  if !ifile = "" then raise (Arg.Bad "No input file")
  else
    let l1 =
      match Artifact.Loader.load (!ifile ^ ".fgir_dump") with
      | Artifact.Data.L1 l1 -> l1
      | _ -> raise (Failure "not l1")
    in
    let l2 =
      match Artifact.Loader.load (!ifile ^ ".asir_dump") with
      | Artifact.Data.L2 l2 -> l2
      | _ -> raise (Failure "not l2")
    in
    let l3 =
      match Artifact.Loader.load (!ifile ^ ".ioir_dump") with
      | Artifact.Data.L3 l3 -> l3
      | _ -> raise (Failure "not l3")
    in
    let main =
      (List.find
         (fun (x : FGIR.Syn.Func.t) ->
           String.equal (Option.value x.nameo ~default:"") "main")
         l1.funcs)
        .entry |> Loc.get_addr
    in

    match Simulation.Check_simulation.run l1 l2 l3 main with
    | Ok () -> Format.printf "Simulation succeeded\n"
    | Error NormalStop -> Format.printf "Simulation succeeded\n"
    | Error (FailStop e) -> [%log info "Simulation failed: %s" e]

let () = Global.run_main main
