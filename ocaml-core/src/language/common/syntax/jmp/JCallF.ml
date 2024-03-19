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

  type t = { target : CallTarget.t; fallthrough : Loc.t; attr : Attr.t }

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
