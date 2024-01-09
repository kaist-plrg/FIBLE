open Basic

type t =
  | Iunimplemented
  | Iload of { space : VarNode.t; pointer : VarNode.t; output : RegId.t_full }
  | Istore of { space : VarNode.t; pointer : VarNode.t; value : VarNode.t }
  | Ijump of Loc.t
  | Ijump_ind of VarNode.t
  | Icbranch of { condition : VarNode.t; target : Loc.t }
  | Iassignment of { expr : Assignable.t; output : RegId.t_full }
  | INop

type t_full = { ins : t; mnem : Mnemonic.t }

let pp (fmt : Format.formatter) (p : t) =
  match p with
  | Iload { space; pointer; output } ->
      Format.fprintf fmt "%a = *[%a]%a;" RegId.pp_full output VarNode.pp space
        VarNode.pp pointer
  | Istore { space; pointer; value } ->
      Format.fprintf fmt "*[%a]%a = %a;" VarNode.pp space VarNode.pp pointer
        VarNode.pp value
  | Ijump i -> Format.fprintf fmt "goto %a;" Loc.pp i
  | Ijump_ind i -> Format.fprintf fmt "goto *%a;" VarNode.pp i
  | Icbranch { condition; target } ->
      Format.fprintf fmt "if %a goto %a;" VarNode.pp condition Loc.pp target
  | Iassignment { expr; output } ->
      Format.fprintf fmt "%a = %a;" RegId.pp_full output Assignable.pp expr
  | INop -> Format.fprintf fmt "nop;"
  | Iunimplemented -> Format.fprintf fmt "unimplemented"

let pp_full (fmt : Format.formatter) (p : t_full) =
  Format.fprintf fmt "%a %a" Mnemonic.pp p.mnem pp p.ins
