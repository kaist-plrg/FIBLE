let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0o666
let kill_enabled = ref false
let pid_list : int list ref = ref []
let projectd : String.t option ref = ref None
let killall () = List.iter (fun pid -> Unix.kill pid Sys.sigterm) !pid_list
let finailize () = if !kill_enabled then killall () else ()

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
