type t

external of_bytes_raw : bytes -> t = "caml_float32_of_bytes"
external to_bytes : t -> bytes = "caml_float32_to_bytes"
external of_string_raw : string -> t = "caml_float32_of_string"
external to_string : t -> string = "caml_float32_to_string"
external show : t -> string = "caml_float32_show"
external read : string -> t = "caml_float32_read"
external equal : t -> t -> bool = "caml_float32_equal"
external compare : t -> t -> int = "caml_float32_compare"
external neg : t -> t = "caml_float32_neg"
external abs : t -> t = "caml_float32_abs"
external sqrt : t -> t = "caml_float32_sqrt"
external ceil : t -> t = "caml_float32_ceil"
external floor : t -> t = "caml_float32_floor"
external round : t -> t = "caml_float32_round"
external is_nan : t -> bool = "caml_float32_is_nan"
external trunc : t -> int64 = "caml_float32_trunc"
external add : t -> t -> t = "caml_float32_add"
external sub : t -> t -> t = "caml_float32_sub"
external mul : t -> t -> t = "caml_float32_mul"
external div : t -> t -> t = "caml_float32_div"

let size : int = 4
let zero : t = read "0.0"

let of_bytes b =
  if Int.compare (Bytes.length b) size < 0 then invalid_arg "Float32.of_bytes";
  of_bytes_raw b

let of_string s =
  if Int.compare (String.length s) size < 0 then invalid_arg "Float32.of_string";
  of_string_raw s

let pp fmt v = Format.fprintf fmt "%s" (show v)
