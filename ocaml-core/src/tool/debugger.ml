open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "debugger <ifile>"
let ifile = ref ""
let port = ref 0
let args = ref []

let speclist =
  [
    ("--", Arg.Rest_all (fun x -> args := x), ": arguments to program");
    ("-p", Arg.Set_int port, ": port");
  ]

let debug_l1 (in_chan, out_chan) l1 args =
  Artifact.Repl.FGIR.repl (in_chan, out_chan) l1
    (FGIR.Init.default l1 args Environment.env)

let debug_l2 (in_chan, out_chan) l2 args =
  Artifact.Repl.ASIR.repl (in_chan, out_chan) l2
    (ASIR.Init.default l2 args Environment.env)

let debug_l3 (in_chan, out_chan) l3 args =
  Artifact.Repl.IOIR.repl (in_chan, out_chan) l3
    (IOIR.Init.default l3 args Environment.env)

let main () =
  Arg.parse speclist
    (fun x ->
      if !ifile = "" then ifile := x else raise (Arg.Bad "too many input files"))
    usage_msg;
  args := !ifile :: !args;
  if !ifile = "" then raise (Arg.Bad "No input file")
  else
    let data = Artifact.Loader.load !ifile in
    let sfd, port = Util.create_server_socket ~default_port:!port () in
    Format.printf "Server on port %d\n%!" port;

    let server_fun in_chan out_chan =
      let res =
        match data with
        | Artifact.Data.L1 l1 -> debug_l1 (in_chan, out_chan) l1 !args
        | Artifact.Data.L2 l2 -> debug_l2 (in_chan, out_chan) l2 !args
        | Artifact.Data.L3 l3 -> debug_l3 (in_chan, out_chan) l3 !args
      in
      match res with
      | Ok _ -> [%log info "Success"]
      | Error e -> [%log error "Error: %s\n" e]
    in
    Util.establish_server server_fun sfd

let () = Global.run_main main
