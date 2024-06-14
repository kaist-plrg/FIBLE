module Make (VarNode : VarNodeF.S) = struct
  type t = { condition : VarNode.t; target : Loc.t } [@@deriving sexp]

  let pp fmt { condition; target } =
    Format.fprintf fmt "if %a goto %a;" VarNode.pp condition Loc.pp target

  let is_nop (_ : t) = false
end
