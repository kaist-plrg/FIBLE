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
    let l3 : L3.Prog.t = L3.Prog.load_prog !ifile in
    (match
       L3.Interp.interp l3
         (L3.Init.from_signature l3
            ((List.find
                (fun (x : L3.Func.t) ->
                  String.equal (Option.value x.nameo ~default:"") "main")
                l3.funcs)
               .entry |> fst))
     with
    | Ok _ -> [%log info "Success"]
    | Error e -> [%log error "Error: %s\n" e]);
    ()

let () = Global.run_main main
