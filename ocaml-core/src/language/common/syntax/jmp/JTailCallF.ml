module Make (CallTarget : sig
  type t

  val pp : Format.formatter -> t -> unit
  val get_loc_opt : t -> Loc.t option
end) (Attr : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  module Attr = Attr

  type t = { target : CallTarget.t; attr : Attr.t }

  let pp fmt ({ target; attr } : t) =
    Format.fprintf fmt "tailcall %a [%a];" CallTarget.pp target Attr.pp attr

  let succ (v : t) = []
  let get_target { target; _ } = target
  let get_attr { attr; _ } = attr
  let resolve_calltarget_opt ({ target; _ } : t) = CallTarget.get_loc_opt target
  let is_ret (v : t) = false
end
