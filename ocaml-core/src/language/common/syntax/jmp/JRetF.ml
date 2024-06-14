module Make (Attr : sig
  type t

  val pp : Format.formatter -> t -> unit
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end) =
struct
  module Attr = Attr

  type t = { attr : Attr.t } [@@deriving sexp]

  let pp fmt ({ attr } : t) = Format.fprintf fmt "return [%a];" Attr.pp attr
  let succ (v : t) = []
  let resolve_calltarget_opt (v : t) = None
  let get_attr { attr } = attr
  let is_ret (v : t) = true
end
