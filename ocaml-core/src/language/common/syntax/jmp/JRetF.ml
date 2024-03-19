module Make (Attr : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  module Attr = Attr

  type t = { attr : Attr.t }

  let pp fmt ({ attr } : t) = Format.fprintf fmt "return [%a];" Attr.pp attr
  let succ (v : t) = []
  let resolve_calltarget_opt (v : t) = None
  let get_attr { attr } = attr
  let is_ret (v : t) = true
end
