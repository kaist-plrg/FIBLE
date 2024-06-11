module Make (VarNode : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = { condition : VarNode.t; target : Loc.t }

  let pp fmt { condition; target } =
    Format.fprintf fmt "if %a goto %a;" VarNode.pp condition Loc.pp target

  let is_nop (_ : t) = false
end
