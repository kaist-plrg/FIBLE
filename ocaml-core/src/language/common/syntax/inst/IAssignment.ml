type 'assignable_t poly_t = { expr : 'assignable_t; output : RegId.t_full }
[@@deriving sexp]

module Make (VarNode : sig
  type t
end) (Assignable : sig
  type t = VarNode.t AssignableF.poly_t

  val pp : Format.formatter -> t -> unit
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end) =
struct
  type t = Assignable.t poly_t [@@deriving sexp]

  let pp fmt ({ expr; output } : t) =
    Format.fprintf fmt "%a = %a;" RegId.pp_full output Assignable.pp expr

  let is_nop (p : t) = false
end
