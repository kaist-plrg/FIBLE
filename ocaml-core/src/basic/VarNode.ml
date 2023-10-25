type t = Register of RegId.t_width | Const of Const.t

let pp (fmt : Format.formatter) (v : t) =
  match v with
  | Register n -> Format.fprintf fmt "%a" RegId.pp_width n
  | Const n -> Format.fprintf fmt "%a" Const.pp n

let compare = compare
let width = function Register n -> n.width | Const n -> n.width
