type t = INop [@@deriving sexp]

let pp fmt (_ : t) = Format.fprintf fmt "nop;"
let is_nop (p : t) = true
