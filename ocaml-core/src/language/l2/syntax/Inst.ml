open Basic

type t =
  | Iload of { space : VarNode.t; pointer : VarNode.t; output : RegId.t_full }
  | Istore of { space : VarNode.t; pointer : VarNode.t; value : VarNode.t }
  | Ilload of { offset : Const.t; output : RegId.t_full }
  | Ilstore of { offset : Const.t; value : VarNode.t }
  | Ipload of { offset : Const.t; output : RegId.t_full }
  | Ipstore of { offset : Const.t; value : VarNode.t }
  | Iassignment of { expr : Assignable.t; output : RegId.t_full }
  | INop

type t_full = { ins : t; loc : Loc.t; mnem : Mnemonic.t }

let pp (fmt : Format.formatter) (p : t) =
  match p with
  | Iload { space; pointer; output } ->
      Format.fprintf fmt "%a = *[%a]%a;" RegId.pp_full output VarNode.pp space
        VarNode.pp pointer
  | Istore { space; pointer; value } ->
      Format.fprintf fmt "*[%a]%a = %a;" VarNode.pp space VarNode.pp pointer
        VarNode.pp value
  | Ilload { offset; output } ->
      Format.fprintf fmt "%a = local[%a];" RegId.pp_full output Const.pp offset
  | Ilstore { offset; value } ->
      Format.fprintf fmt "local[%a] = %a;" Const.pp offset VarNode.pp value
  | Ipload { offset; output } ->
      Format.fprintf fmt "%a = param[%a];" RegId.pp_full output Const.pp offset
  | Ipstore { offset; value } ->
      Format.fprintf fmt "param[%a] = %a;" Const.pp offset VarNode.pp value
  | Iassignment { expr; output } ->
      Format.fprintf fmt "%a = %a;" RegId.pp_full output Assignable.pp expr
  | INop -> Format.fprintf fmt "nop;"

let pp_full (fmt : Format.formatter) (p : t_full) =
  Format.fprintf fmt "%a: %a [%a]" Loc.pp p.loc pp p.ins Mnemonic.pp p.mnem
