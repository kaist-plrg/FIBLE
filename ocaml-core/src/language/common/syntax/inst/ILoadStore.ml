type 'varnode_t poly_t =
  | Load of { space : 'varnode_t; pointer : 'varnode_t; output : RegId.t_full }
  | Store of { space : 'varnode_t; pointer : 'varnode_t; value : 'varnode_t }
[@@deriving sexp]

module Make (VarNode : sig
  type t

  val pp : Format.formatter -> t -> unit
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end) =
struct
  type t = VarNode.t poly_t [@@deriving sexp]

  let pp (fmt : Format.formatter) (p : t) =
    match p with
    | Load { space; pointer; output } ->
        Format.fprintf fmt "%a = *[%a]%a;" RegId.pp_full output VarNode.pp space
          VarNode.pp pointer
    | Store { space; pointer; value } ->
        Format.fprintf fmt "*[%a]%a = %a;" VarNode.pp space VarNode.pp pointer
          VarNode.pp value

  let is_nop (p : t) = false
end
