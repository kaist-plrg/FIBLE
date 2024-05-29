type t = IUnimplemented

let pp fmt (_ : t) = Format.fprintf fmt "unimplemented;"
let is_nop (_ : t) = false
