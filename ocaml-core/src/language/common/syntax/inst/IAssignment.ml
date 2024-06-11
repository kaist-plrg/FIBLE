type 'assignable_t poly_t = { expr : 'assignable_t; output : RegId.t_full }

module Make (VarNode : sig
  type t
end) (Assignable : sig
  type t = VarNode.t AssignableF.poly_t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = Assignable.t poly_t

  let pp fmt ({ expr; output } : t) =
    Format.fprintf fmt "%a = %a;" RegId.pp_full output Assignable.pp expr

  let is_nop (p : t) = false
end
