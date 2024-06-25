open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "interpreter -i <ifile>"
let ifile = ref ""
let cwd = ref ""

let speclist =
  [
    ("-i", Arg.Set_string ifile, ": input file");
    ("-project-cwd", Arg.Set_string cwd, ": set cwd");
    ("-debug", Arg.Unit (fun _ -> Logger.set_level Logger.Debug), ": debug mode");
    ("-log-path", Arg.String (fun x -> Logger.set_log_file x), ": log path");
    ( "-log-feature",
      Arg.String (fun x -> Logger.add_log_feature x),
      ": add log feature" );
  ]

let interp_l1 l1 =
  match
    FGIR.Interp.interp l1
      (FGIR.Init.from_signature l1
         ((List.find
             (fun (x : FGIR.Syn.Func.t) ->
               String.equal (Option.value x.nameo ~default:"") "main")
             l1.funcs)
            .entry |> Loc.get_addr))
  with
  | Ok _ | Error NormalStop -> ()
  | Error (FailStop e) -> [%log error "Error: %s\n" e]

let interp_l2 l2 =
  match
    ASIR.Interp.interp l2
      (ASIR.Init.from_signature l2
         ((List.find
             (fun (x : ASIR.Syn.Func.t) ->
               String.equal (Option.value x.nameo ~default:"") "main")
             l2.funcs)
            .entry |> Loc.get_addr))
  with
  | Ok _ | Error NormalStop -> ()
  | Error (FailStop e) -> [%log error "Error: %s\n" e]

let interp_l3 l3 =
  (match IOIR.Interp.interp l3 (IOIR.Init.default l3) with
  | Ok _ | Error NormalStop -> ()
  | Error (FailStop e) -> [%log error "Error: %s\n" e]);
  ()

let main () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage_msg;
  if !ifile = "" then raise (Arg.Bad "No input file")
  else
    let data = Artifact.Loader.load !ifile in
    match data with
    | Artifact.Data.L1 l1 -> interp_l1 l1
    | Artifact.Data.L2 l2 -> interp_l2 l2
    | Artifact.Data.L3 l3 -> interp_l3 l3

let () = Global.run_main main
