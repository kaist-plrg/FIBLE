type os_type = Linux | Darwin | Unsupported

let os_type =
  let ic, oc = Unix.open_process "uname -s" in
  let v =
    ic |> input_line |> function
    | "Linux" -> Linux
    | "Darwin" -> Darwin
    | _ -> Unsupported
  in
  ignore (Unix.close_process (ic, oc));
  v

let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0o666
let kill_enabled = ref false
let global_blk_offset = ref 0xbaaaffff00000000L
let pid_list : int list ref = ref []
let projectd : String.t = [%pwd]
let cgc_lib : Dl.library option ref = ref None

let cgc_lib_path : String.t =
  Format.asprintf "%s/cgc/lib/libcgc/%s" projectd
    (match os_type with
    | Linux -> "libcgc.so"
    | Darwin -> "libcgc.dylib"
    | Unsupported -> [%log fatal "Unsupported OS"])

let initialize_cgc_lib () =
  match !cgc_lib with
  | None ->
      cgc_lib := Some (Dl.dlopen ~filename:cgc_lib_path ~flags:[ Dl.RTLD_NOW ])
  | Some _ -> ()

let killall () = List.iter (fun pid -> Unix.kill pid Sys.sigterm) !pid_list

let finailize () =
  (* Printexc.print_backtrace stdout; *)
  if !kill_enabled then killall () else ()

let install_pid pid =
  pid_list := pid :: !pid_list;
  if not !kill_enabled then (
    kill_enabled := true;
    Sys.set_signal Sys.sigint
      (Sys.Signal_handle
         (fun _ ->
           killall ();
           exit 0));
    Sys.set_signal Sys.sigterm
      (Sys.Signal_handle
         (fun _ ->
           killall ();
           exit 0)))
  else ()

let run_main (main : unit -> 'a) : 'a = Fun.protect ~finally:finailize main
