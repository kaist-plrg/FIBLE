type ('calltarget_t, 'attr_t) poly_t = {
  target : 'calltarget_t;
  fallthrough : Loc.t;
  attr : 'attr_t;
}
[@@deriving sexp]

module Make (CallTarget : sig
  type t

  val pp : Format.formatter -> t -> unit
  val get_loc_opt : t -> Loc.t option
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end) (Attr : sig
  type t

  val pp : Format.formatter -> t -> unit
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end) =
struct
  module Attr = Attr

  type t = (CallTarget.t, Attr.t) poly_t [@@deriving sexp]

  let pp fmt (p : t) =
    Format.fprintf fmt "call %a [%a]; -> %a" CallTarget.pp p.target Loc.pp
      p.fallthrough Attr.pp p.attr

  let succ (jmp : t) : Loc.t List.t = [ jmp.fallthrough ]
  let get_target { target; _ } = target
  let get_fallthrough { fallthrough; _ } = fallthrough
  let get_attr { attr; _ } = attr
  let resolve_calltarget_opt { target } = CallTarget.get_loc_opt target
  let is_ret (v : t) = false
  let get_target (jmp : t) = jmp.target
end
