type t = Neg | Not

let pp (fmt : Format.formatter) (op : t) : unit =
  match op with Neg -> Format.fprintf fmt "-" | Not -> Format.fprintf fmt "!"
