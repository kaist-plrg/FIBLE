open Basic

type t =
  | Iunimplemented
  | Iload of (VarNode.t * VarNode.t * VarNode.t)
  | Istore of (VarNode.t * VarNode.t * VarNode.t)
  | Ijump of Loc.t
  | Ijump_ind of VarNode.t
  | Icbranch of (VarNode.t * Loc.t)
  | Iassignment of (Assignable.t * VarNode.t)
  | INop

type t_full = { ins : t; mnem : Mnemonic.t }

let pp (fmt : Format.formatter) (p : t) =
  match p with
  | Iload (i0, i1, o) ->
      Format.fprintf fmt "%a = *[%a]%a;" VarNode.pp o VarNode.pp i0 VarNode.pp
        i1
  | Istore (i0, i1, i2) ->
      Format.fprintf fmt "*[%a]%a = %a;" VarNode.pp i0 VarNode.pp i1 VarNode.pp
        i2
  | Ijump i -> Format.fprintf fmt "goto %a;" Loc.pp i
  | Ijump_ind i -> Format.fprintf fmt "goto *%a;" VarNode.pp i
  | Icbranch (i0, i1) ->
      Format.fprintf fmt "if %a goto %a;" VarNode.pp i0 Loc.pp i1
  | Iassignment (i, o) ->
      Format.fprintf fmt "%a = %a;" VarNode.pp o Assignable.pp i
  | INop -> Format.fprintf fmt "nop;"
  | Iunimplemented -> Format.fprintf fmt "unimplemented"

let pp_full (fmt : Format.formatter) (p : t_full) =
  Format.fprintf fmt "%a %a" Mnemonic.pp p.mnem pp p.ins
