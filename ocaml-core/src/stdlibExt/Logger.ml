type level = Debug | Info | Warn | Error | Fatal

exception Error
exception Fatal

let map_level = function
  | Debug -> 0
  | Info -> 1
  | Warn -> 2
  | Error -> 3
  | Fatal -> 4

let level_to_string = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"
  | Fatal -> "FATAL"

let le (l : level) (r : level) = map_level l <= map_level r
let log_formatter = ref Format.std_formatter
let current_level = ref Info
let log_features = ref []
let set_level level = current_level := level

let pp_time fmt
    ({ tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _ } : Unix.tm) =
  Format.fprintf fmt "%04d-%02d-%02d_%02d:%02d:%02d" (tm_year + 1900)
    (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let set_log_file fname =
  log_formatter :=
    Format.formatter_of_out_channel
      (Stdlib.open_out_gen [ Open_wronly; Open_creat; Open_append ] 0o664 fname)

let add_log_feature feature = log_features := feature :: !log_features

let log (fname : String.t) (line : Int.t) (level : level)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  let formatter = !log_formatter in
  if le !current_level level then (
    Format.fprintf formatter "[%s] [%a] %s:%d: " (level_to_string level) pp_time
      (Unix.time () |> Unix.localtime)
      fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ())
      formatter fstr)
  else Format.ifprintf formatter fstr

let flog (fname : String.t) (line : Int.t) (level : level) (feature : String.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  let formatter = !log_formatter in
  if List.mem feature !log_features then log fname line level fstr
  else Format.ifprintf formatter fstr

let debug (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  log fname line Debug fstr

let fdebug (fname : String.t) (line : Int.t) (feature : String.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  flog fname line Debug feature fstr

let info (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  log fname line Info fstr

let finfo (fname : String.t) (line : Int.t) (feature : String.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  flog fname line Info feature fstr

let warn (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  log fname line Warn fstr

let fwarn (fname : String.t) (line : Int.t) (feature : String.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  flog fname line Warn feature fstr

let error (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit, 'b) format4) : 'a =
  let formatter = !log_formatter in
  if le !current_level Error then (
    Format.fprintf formatter "[ERROR] [%a] %s:%d: " pp_time
      (Unix.time () |> Unix.localtime)
      fname line;
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
    Format.fprintf formatter "[FATAL] [%a] %s:%d: " pp_time
      (Unix.time () |> Unix.localtime)
      fname line;
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
    Format.fprintf formatter "[ERROR] [%a] %s:%d: %s" pp_time
      (Unix.time () |> Unix.localtime)
      fname line (Printexc.to_string e);
    Format.pp_print_cut formatter ();
    Format.pp_print_flush formatter ())
  else ();
  Stdlib.raise e

let annotate (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit, string) format4) : 'a =
  let (format1 : (string -> int -> 'a, Format.formatter, unit, 'a) format4) =
    "%s:%d: "
  in
  let format2 = fstr in
  let fstr = format1 ^^ format2 in
  Format.asprintf fstr fname line
