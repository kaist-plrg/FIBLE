open Basic

module Make (CallTarget : sig
  type t

  val pp : Format.formatter -> t -> unit
  val get_loc_opt : t -> Loc.t option
end) (Attr : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = { target : CallTarget.t; attr : Attr.t }

  let pp fmt ({ target; attr } : t) =
    Format.fprintf fmt "tailcall %a [%a];" CallTarget.pp target Attr.pp attr

  let succ (v : t) = []
  let get_call_target ({ target; _ } : t) = CallTarget.get_loc_opt target
  let is_ret (v : t) = false
end
