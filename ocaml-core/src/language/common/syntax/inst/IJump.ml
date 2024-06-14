type t = { target : Loc.t } [@@deriving sexp]

let pp fmt { target } = Format.fprintf fmt "goto %a;" Loc.pp target
let is_nop (_ : t) = false
