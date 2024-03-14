type t = Register of RegId.t_full | Const of Const.t | Ram of Const.t

let pp (fmt : Format.formatter) (v : t) =
  match v with
  | Register n -> Format.fprintf fmt "%a" RegId.pp_full n
  | Ram n -> Format.fprintf fmt "*[ram]%a" Const.pp n
  | Const n -> Format.fprintf fmt "%a" Const.pp n

let compare = compare

let width = function
  | Register n -> n.width
  | Const n -> n.width
  | Ram n -> n.width
