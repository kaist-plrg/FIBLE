type t = IUnimplemented [@@deriving sexp]

let pp fmt (_ : t) = Format.fprintf fmt "unimplemented;"
let is_nop (_ : t) = false
