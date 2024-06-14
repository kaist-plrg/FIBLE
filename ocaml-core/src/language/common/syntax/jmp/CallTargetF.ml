module Make
    (VarNode : VarNodeF.S)
    (Attr : sig
      type t

      val pp : Format.formatter -> t -> unit
      val t_of_sexp : Sexplib.Sexp.t -> t
      val sexp_of_t : t -> Sexplib.Sexp.t
    end) =
struct
  module Attr = Attr

  type t =
    | Cdirect of { target : Loc.t; attr : Attr.t }
    | Cind of { target : VarNode.t }
  [@@deriving sexp]

  let pp fmt = function
    | Cdirect { target; attr } ->
        Format.fprintf fmt "%a [%a]" Loc.pp target Attr.pp attr
    | Cind { target } -> Format.fprintf fmt "*%a" VarNode.pp target

  let get_loc_opt = function Cdirect v -> Some v.target | Cind _ -> None

  let to_either (v : t) : (Loc.t * Attr.t, VarNode.t) Either.t =
    match v with
    | Cdirect { target; attr } -> Left (target, attr)
    | Cind { target } -> Right target
end
