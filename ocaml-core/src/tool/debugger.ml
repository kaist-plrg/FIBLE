open StdlibExt
open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "debugger -i <ifile>"
let ifile = ref ""
let port = ref 0

let speclist =
  [
    ("-i", Arg.Set_string ifile, ": input file");
    ("-p", Arg.Set_int port, ": port");
  ]

let debug_l1 l1 = failwith "not implemented"
let debug_l2 l2 = failwith "not implemented"

let debug_l3 (in_chan, out_chan) l3 =
  Artifact.L3_repl.repl (in_chan, out_chan) l3 (L3.Init.default l3)

let main () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage_msg;
  if !ifile = "" then raise (Arg.Bad "No input file")
  else
    let data = Artifact.Loader.load !ifile in
    let sfd, port = Util.create_server_socket ~default_port:!port () in
    Format.printf "Server on port %d\n%!" port;

    let server_fun in_chan out_chan =
      let res =
        match data with
        | Artifact.Data.L1 l1 -> debug_l1 (in_chan, out_chan) l1
        | Artifact.Data.L2 l2 -> debug_l2 (in_chan, out_chan) l2
        | Artifact.Data.L3 l3 -> debug_l3 (in_chan, out_chan) l3
      in
      match res with
      | Ok _ -> [%log info "Success"]
      | Error e -> [%log error "Error: %s\n" e]
    in
    Util.establish_server server_fun sfd

let () = Global.run_main main
