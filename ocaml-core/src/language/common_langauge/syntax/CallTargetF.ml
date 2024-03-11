open Basic

module Make (Attr : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  module Attr = Attr

  type t =
    | Cdirect of { target : Loc.t; attr : Attr.t }
    | Cind of { target : VarNode.t }

  type resolved_t = { target : Loc.t; attr_opt : Attr.t option }

  let pp fmt = function
    | Cdirect { target; attr } ->
        Format.fprintf fmt "%a [%a]" Loc.pp target Attr.pp attr
    | Cind { target } -> Format.fprintf fmt "*%a" VarNode.pp target

  let get_loc_opt = function Cdirect v -> Some v.target | Cind _ -> None

  let to_either (v : t) : (Loc.t * Attr.t, VarNode.t) Either.t =
    match v with
    | Cdirect { target; attr } -> Left (target, attr)
    | Cind { target } -> Right target

  let mk_direct (target : Loc.t) (attr : Attr.t) : resolved_t =
    { target; attr_opt = Some attr }

  let mk_indirect (target : Loc.t) : resolved_t = { target; attr_opt = None }
  let get_target_resolved (v : resolved_t) : Loc.t = v.target
end
