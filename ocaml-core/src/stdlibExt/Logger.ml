
type level = Debug | Info | Warn | Error | Fatal

let map_level = function
  | Debug -> 0
  | Info -> 1
  | Warn -> 2
  | Error -> 3
  | Fatal -> 4

let le (l: level) (r: level) =
  map_level l <= map_level r

let current_level = ref Info

let set_level level =
  current_level := level

let debug (fstr: ('a, Format.formatter, unit) format): 'a =
  if le !current_level Debug then
    (Format.fprintf Format.std_formatter "[DEBUG] ";
    Format.fprintf Format.std_formatter fstr)
  else
    Format.ifprintf Format.std_formatter fstr

let info (fstr: ('a, Format.formatter, unit) format): 'a =
  if le !current_level Info then
    (Format.fprintf Format.std_formatter "[INFO] ";
    Format.fprintf Format.std_formatter fstr)
  else
    Format.ifprintf Format.std_formatter fstr

let warn (fstr: ('a, Format.formatter, unit) format): 'a =
  if le !current_level Warn then
    (Format.fprintf Format.std_formatter "[WARN] ";
    Format.fprintf Format.std_formatter fstr)
  else
    Format.ifprintf Format.std_formatter fstr

let error (fstr: ('a, Format.formatter, unit) format): 'a  =
  if le !current_level Error then
    (Format.fprintf Format.std_formatter "[ERROR] ";
    Format.fprintf Format.std_formatter fstr)
  else
    Format.ifprintf Format.std_formatter fstr

let fatal (fstr: ('a, Format.formatter, unit) format): 'a  =
  if le !current_level Fatal then
    (Format.fprintf Format.std_formatter "[FATAL] ";
    Format.fprintf Format.std_formatter fstr)
  else
    Format.ifprintf Format.std_formatter fstr