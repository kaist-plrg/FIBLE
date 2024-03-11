open Basic

module Make (CallTarget : sig
  type t
  type resolved_t

  val pp : Format.formatter -> t -> unit
  val get_loc_opt : t -> Loc.t option
end) (Attr : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  module Attr = Attr

  type resolved_t = {
    target : CallTarget.resolved_t;
    fallthrough : Loc.t;
    attr : Attr.t;
  }

  type t = { target : CallTarget.t; fallthrough : Loc.t; attr : Attr.t }

  let pp fmt (p : t) =
    Format.fprintf fmt "call %a [%a]; -> %a" CallTarget.pp p.target Loc.pp
      p.fallthrough Attr.pp p.attr

  let succ (jmp : t) : Loc.t List.t = [ jmp.fallthrough ]
  let get_call_target { target; _ } = CallTarget.get_loc_opt target
  let is_ret (v : t) = false
  let get_target (jmp : t) = jmp.target
  let get_target_resolved (jmp : resolved_t) = jmp.target
  let get_attr_resolved (jmp : resolved_t) = jmp.attr
  let get_fallthrough_resolved (jmp : resolved_t) = jmp.fallthrough

  let to_resolved (jc : t) (target : CallTarget.resolved_t) : resolved_t =
    { target; fallthrough = jc.fallthrough; attr = jc.attr }
end
