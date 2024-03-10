open Basic

type t =
  | Sload of { offset : Const.t; output : RegId.t_full }
  | Sstore of { offset : Const.t; value : VarNode.t }

let pp (fmt : Format.formatter) (p : t) =
  match p with
  | Sload { offset; output } ->
      Format.fprintf fmt "%a = stack[%a];" RegId.pp_full output Const.pp offset
  | Sstore { offset; value } ->
      Format.fprintf fmt "stack[%a] = %a;" Const.pp offset VarNode.pp value

let is_nop (p : t) = false
