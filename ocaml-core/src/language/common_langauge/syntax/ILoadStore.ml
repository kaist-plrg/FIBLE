open Basic

type t =
  | Load of { space : VarNode.t; pointer : VarNode.t; output : RegId.t_full }
  | Store of { space : VarNode.t; pointer : VarNode.t; value : VarNode.t }

let pp (fmt : Format.formatter) (p : t) =
  match p with
  | Load { space; pointer; output } ->
      Format.fprintf fmt "%a = *[%a]%a;" RegId.pp_full output VarNode.pp space
        VarNode.pp pointer
  | Store { space; pointer; value } ->
      Format.fprintf fmt "*[%a]%a = %a;" VarNode.pp space VarNode.pp pointer
        VarNode.pp value

let is_nop (p : t) = false
