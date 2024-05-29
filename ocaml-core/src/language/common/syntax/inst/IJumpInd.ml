type t = { target : VarNode.t }

let pp fmt { target } = Format.fprintf fmt "goto *%a;" VarNode.pp target
let is_nop (_ : t) = false
