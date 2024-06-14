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

  type t = { target : CallTarget.t; attr : Attr.t } [@@deriving sexp]

  let pp fmt ({ target; attr } : t) =
    Format.fprintf fmt "tailcall %a [%a];" CallTarget.pp target Attr.pp attr

  let succ (v : t) = []
  let get_target { target; _ } = target
  let get_attr { attr; _ } = attr
  let resolve_calltarget_opt ({ target; _ } : t) = CallTarget.get_loc_opt target
  let is_ret (v : t) = false
end
