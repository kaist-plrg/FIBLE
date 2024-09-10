type t

external of_bytes_raw : bytes -> t = "caml_float80_of_bytes"
external to_bytes : t -> bytes = "caml_float80_to_bytes"
external of_string_raw : string -> t = "caml_float80_of_string"
external to_string : t -> string = "caml_float80_to_string"
external show : t -> string = "caml_float80_show"
external read : string -> t = "caml_float80_read"
external equal : t -> t -> bool = "caml_float80_equal"
external compare : t -> t -> int = "caml_float80_compare"
external neg : t -> t = "caml_float80_neg"
external abs : t -> t = "caml_float80_abs"
external sqrt : t -> t = "caml_float80_sqrt"
external ceil : t -> t = "caml_float80_ceil"
external floor : t -> t = "caml_float80_floor"
external round : t -> t = "caml_float80_round"
external is_nan : t -> bool = "caml_float80_is_nan"
external trunc : t -> int64 = "caml_float80_trunc"
external add : t -> t -> t = "caml_float80_add"
external sub : t -> t -> t = "caml_float80_sub"
external mul : t -> t -> t = "caml_float80_mul"
external div : t -> t -> t = "caml_float80_div"

let size : int = 10
let zero : t = read "0.0"

let of_bytes b =
  if Int.compare (Bytes.length b) size < 0 then invalid_arg "Float80.of_bytes";
  of_bytes_raw b

let of_string s =
  if Int.compare (String.length s) size < 0 then invalid_arg "Float80.of_string";
  of_string_raw s

let pp fmt v = Format.fprintf fmt "%s" (show v)
