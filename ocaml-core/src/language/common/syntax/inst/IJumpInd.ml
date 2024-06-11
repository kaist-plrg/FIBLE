module Make (VarNode : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = { target : VarNode.t }

  let pp fmt { target } = Format.fprintf fmt "goto *%a;" VarNode.pp target
  let is_nop (_ : t) = false
end
