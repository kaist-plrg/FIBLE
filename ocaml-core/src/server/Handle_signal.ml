let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0o666
let enabled = ref false
let pid_list : int list ref = ref []
let killall () = List.iter (fun pid -> Unix.kill pid Sys.sigterm) !pid_list
let finailize () = if !enabled then killall () else ()

let install_pid pid =
  pid_list := pid :: !pid_list;
  if not !enabled then (
    enabled := true;
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
