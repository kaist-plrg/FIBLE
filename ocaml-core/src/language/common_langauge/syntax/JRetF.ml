open Basic

module Make (Attr : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = { attr : Attr.t }

  let pp fmt ({ attr } : t) = Format.fprintf fmt "return [%a];" Attr.pp attr
  let succ (v : t) = []
  let get_call_target (v : t) = None
  let is_ret (v : t) = true
end
