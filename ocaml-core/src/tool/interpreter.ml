open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "interpreter <ifile>"
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

let interp_l1 l1 args =
  match FGIR.Interp.interp l1 (FGIR.Init.default l1 args) with
  | Ok _ | Error NormalStop -> ()
  | Error (FailStop e) -> [%log error "Error: %s\n" e]

let interp_l2 l2 args =
  match ASIR.Interp.interp l2 (ASIR.Init.default l2 args) with
  | Ok _ | Error NormalStop -> ()
  | Error (FailStop e) -> [%log error "Error: %s\n" e]

let interp_l3 l3 args =
  (match IOIR.Interp.interp l3 (IOIR.Init.default l3 args) with
  | Ok _ | Error NormalStop -> ()
  | Error (FailStop e) -> [%log error "Error: %s\n" e]);
  ()

let main () =
  Arg.parse speclist
    (fun x ->
      if !ifile = "" then ifile := x else raise (Arg.Bad "too many input files"))
    usage_msg;
  if !argv0 = "" then argv0 := !ifile;
  args := !argv0 :: !args;
  if !ifile = "" then raise (Arg.Bad "No input file")
  else
    let data = Artifact.Loader.load !ifile in
    match data with
    | Artifact.Data.L1 l1 -> interp_l1 l1 !args
    | Artifact.Data.L2 l2 -> interp_l2 l2 !args
    | Artifact.Data.L3 l3 -> interp_l3 l3 !args

let () = Global.run_main main
