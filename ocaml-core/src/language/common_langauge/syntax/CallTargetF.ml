open Basic

module Make (Attr : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t =
    | Cdirect of { target : Loc.t; attr : Attr.t }
    | Cind of { target : VarNode.t }

  type resolved_t = { target : Loc.t; attr_opt : Attr.t option }

  let pp fmt = function
    | Cdirect { target; attr } ->
        Format.fprintf fmt "%a [%a]" Loc.pp target Attr.pp attr
    | Cind { target } -> Format.fprintf fmt "*%a" VarNode.pp target

  let get_loc_opt = function Cdirect v -> Some v.target | Cind _ -> None
end
