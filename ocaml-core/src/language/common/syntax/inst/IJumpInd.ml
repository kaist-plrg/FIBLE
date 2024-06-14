module Make (VarNode : VarNodeF.S) = struct
  type t = { target : VarNode.t } [@@deriving sexp]

  let pp fmt { target } = Format.fprintf fmt "goto *%a;" VarNode.pp target
  let is_nop (_ : t) = false
end
