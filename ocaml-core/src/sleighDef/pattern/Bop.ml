type t = Add | Sub | Mult | Lshift | Rshift | And | Or | Xor | Div

let pp (fmt : Format.formatter) (op : t) : unit =
  match op with
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"
  | Mult -> Format.fprintf fmt "*"
  | Lshift -> Format.fprintf fmt "<<"
  | Rshift -> Format.fprintf fmt ">>"
  | And -> Format.fprintf fmt "&"
  | Or -> Format.fprintf fmt "|"
  | Xor -> Format.fprintf fmt "^"
  | Div -> Format.fprintf fmt "/"
