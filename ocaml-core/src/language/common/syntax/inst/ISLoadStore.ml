type 'varnode_t poly_t =
  | Sload of { offset : Byte8.t; output : RegId.t_full }
  | Sstore of { offset : Byte8.t; value : 'varnode_t }

module Make (VarNode : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = VarNode.t poly_t

  let pp (fmt : Format.formatter) (p : t) =
    match p with
    | Sload { offset; output } ->
        Format.fprintf fmt "%a = stack[%a];" RegId.pp_full output Byte8.pp
          offset
    | Sstore { offset; value } ->
        Format.fprintf fmt "stack[%a] = %a;" Byte8.pp offset VarNode.pp value

  let is_nop (p : t) = false
end
