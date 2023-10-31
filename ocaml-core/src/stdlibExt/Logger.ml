type level = Debug | Info | Warn | Error | Fatal

let map_level = function
  | Debug -> 0
  | Info -> 1
  | Warn -> 2
  | Error -> 3
  | Fatal -> 4

let le (l : level) (r : level) = map_level l <= map_level r
let current_level = ref Info
let set_level level = current_level := level

let debug (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  if le !current_level Debug then (
    Format.fprintf Format.std_formatter "[DEBUG] %s:%d: " fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ())
      Format.std_formatter fstr)
  else Format.ifprintf Format.std_formatter fstr

let info (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  if le !current_level Info then (
    Format.fprintf Format.std_formatter "[INFO] %s:%d: " fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ())
      Format.std_formatter fstr)
  else Format.ifprintf Format.std_formatter fstr

let warn (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  if le !current_level Warn then (
    Format.fprintf Format.std_formatter "[WARN] %s:%d: " fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ())
      Format.std_formatter fstr)
  else Format.ifprintf Format.std_formatter fstr

let error (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  if le !current_level Error then (
    Format.fprintf Format.std_formatter "[ERROR] %s:%d: " fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ())
      Format.std_formatter fstr)
  else Format.ifprintf Format.std_formatter fstr

let fatal (fname : String.t) (line : Int.t)
    (fstr : ('a, Format.formatter, unit) format) : 'a =
  if le !current_level Fatal then (
    Format.fprintf Format.std_formatter "[FATAL] %s:%d: " fname line;
    Format.kfprintf
      (fun fmt ->
        Format.pp_print_cut fmt ();
        Format.pp_print_flush fmt ())
      Format.std_formatter fstr)
  else Format.ifprintf Format.std_formatter fstr
