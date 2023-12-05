type level = Debug | Info | Warn | Error | Fatal

exception Error
exception Fatal

let map_level = function
  | Debug -> 0
  | Info -> 1
  | Warn -> 2
  | Error -> 3
  | Fatal -> 4

let le (l : level) (r : level) = map_level l <= map_level r
let log_formatter = ref Format.std_formatter
let current_level = ref Info
let set_level level = current_level := level

let set_log_file fname =
  log_formatter := Format.formatter_of_out_channel (Stdlib.open_out fname)

let debug (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  let formatter = !log_formatter in
  if le !current_level Debug then (
    Format.fprintf formatter "[DEBUG] %s:%d: " fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ())
      formatter fstr)
  else Format.ifprintf formatter fstr

let info (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  let formatter = !log_formatter in
  if le !current_level Info then (
    Format.fprintf formatter "[INFO] %s:%d: " fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ())
      formatter fstr)
  else Format.ifprintf formatter fstr

let warn (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  let formatter = !log_formatter in
  if le !current_level Warn then (
    Format.fprintf formatter "[WARN] %s:%d: " fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ())
      formatter fstr)
  else Format.ifprintf formatter fstr

let error (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit, 'b) format4) : 'a =
  let formatter = !log_formatter in
  if le !current_level Error then (
    Format.fprintf formatter "[ERROR] %s:%d: " fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ();
        Stdlib.raise Error)
      formatter fstr)
  else Format.ikfprintf (fun fmt -> Stdlib.raise Error) formatter fstr

let fatal (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit, 'b) format4) : 'a =
  let formatter = !log_formatter in
  if le !current_level Fatal then (
    Format.fprintf formatter "[FATAL] %s:%d: " fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ();
        Stdlib.raise Fatal)
      formatter fstr)
  else Format.ikfprintf (fun fmt -> Stdlib.raise Fatal) formatter fstr

let raise (fname : String.t) (line : Int.t) (e : exn) : 'a =
  let formatter = !log_formatter in
  if le !current_level Error then (
    Format.fprintf formatter "[ERROR] %s:%d: %s" fname line
      (Printexc.to_string e);
    Format.pp_print_cut formatter ();
    Format.pp_print_flush formatter ())
  else ();
  Stdlib.raise e
