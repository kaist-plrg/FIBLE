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
end
