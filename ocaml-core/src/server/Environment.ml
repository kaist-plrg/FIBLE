open StdlibExt
open Basic
open Util

type t = { pid : int; fd_in : Unix.file_descr; fd_out : Unix.file_descr }

let run_environment cwd =
  let environment_path =
    Filename.concat cwd "_build/default/c-core/src/environment.exe"
  in
  let fd_out, fd_in = Unix.pipe ~cloexec:true () in
  let environment_pid =
    Unix.create_process environment_path [| environment_path |] fd_in fd_out
      Handle_signal.devnull
  in
  Logger.debug "Running environment_pid at pid %d\n" environment_pid;
  Handle_signal.install_pid environment_pid;
  (environment_pid, fd_in, fd_out)

let make_server cwd : t =
  let environment_pid, fd_in, fd_out = run_environment cwd in

  { pid = environment_pid; fd_in; fd_out }
