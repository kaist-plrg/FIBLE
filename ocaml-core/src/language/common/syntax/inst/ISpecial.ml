type t = String.t [@@deriving sexp]

let pp fmt (v : t) = Format.fprintf fmt "%s;" v
let is_nop (_ : t) = false
