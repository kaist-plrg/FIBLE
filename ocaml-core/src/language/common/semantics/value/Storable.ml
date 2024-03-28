type t = Byte of Char.t | Undef

let compare a b =
  match (a, b) with
  | Byte a, Byte b -> Char.compare a b
  | Undef, Undef -> 0
  | Undef, _ -> -1
  | _, Undef -> 1

let subsume a b =
  match (a, b) with
  | Byte a, Byte b -> Char.equal a b
  | _, Undef -> true
  | Undef, _ -> false

let try_byte = function Byte c -> Ok c | Undef -> Error "Undefined byte"

let pp fmt c =
  match c with
  | Byte c -> Format.fprintf fmt "%02x" (Char.code c)
  | Undef -> Format.fprintf fmt "xx"
