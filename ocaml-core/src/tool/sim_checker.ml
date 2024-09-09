open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "sim_checker <ifile>"
let ifile = ref ""
let cwd = ref ""
let args = ref []
let argv0 = ref ""

let speclist =
  [
    ("--", Arg.Rest_all (fun x -> args := x), ": arguments to program");
    ("--argv0", Arg.Set_string argv0, ": set argv0");
    ("-project-cwd", Arg.Set_string cwd, ": set cwd");
    ("-debug", Arg.Unit (fun _ -> Logger.set_level Logger.Debug), ": debug mode");
    ("-log-path", Arg.String (fun x -> Logger.set_log_file x), ": log path");
    ( "-log-feature",
      Arg.String (fun x -> Logger.add_log_feature x),
      ": add log feature" );
  ]

let main () =
  Arg.parse speclist
    (fun x ->
      if !ifile = "" then ifile := x else raise (Arg.Bad "too many input files"))
    usage_msg;
  if !argv0 = "" then argv0 := !ifile;
  args := !argv0 :: !args;
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

    match
      Simulation.Check_simulation.run l1 l2 l3 !args Environment.env main
    with
    | Ok () -> Format.printf "Simulation succeeded\n"
    | Error NormalStop -> Format.printf "Simulation succeeded\n"
    | Error (FailStop e) -> [%log info "Simulation failed: %s" e]

let () = Global.run_main main
