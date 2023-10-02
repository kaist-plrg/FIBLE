type t =
  | Iunimplemented
  | Iload of (VarNode.t * VarNode.t * VarNode.t)
  | Istore of (VarNode.t * VarNode.t * VarNode.t)
  | Ijump of (Jannotation.t * VarNode.t)
  | Ijump_ind of (Jiannotation.t * VarNode.t)
  | Icbranch of (VarNode.t * VarNode.t)
  | Iassignment of (Assignable.t * VarNode.t)
  | INop

let pp (fmt : Format.formatter) (p : t) =
  match p with
  | Iload (i0, i1, o) ->
      Format.fprintf fmt "%a = *[%a]%a;" VarNode.pp o VarNode.pp i0 VarNode.pp
        i1
  | Istore (i0, i1, i2) ->
      Format.fprintf fmt "*[%a]%a = %a;" VarNode.pp i0 VarNode.pp i1 VarNode.pp
        i2
  | Ijump (a, i) -> (
      match a with
      | Jbranch -> Format.fprintf fmt "goto %a;" VarNode.pp i
      | Jcall -> Format.fprintf fmt "call %a;" VarNode.pp i)
  | Ijump_ind (a, i) -> (
      match a with
      | JIbranch -> Format.fprintf fmt "goto *%a;" VarNode.pp i
      | JIcall -> Format.fprintf fmt "call *%a;" VarNode.pp i
      | JIret -> Format.fprintf fmt "return %a;" VarNode.pp i)
  | Icbranch (i0, i1) ->
      Format.fprintf fmt "if %a goto %a;" VarNode.pp i0 VarNode.pp i1
  | Iassignment (i, o) ->
      Format.fprintf fmt "%a = %a;" VarNode.pp o Assignable.pp i
  | INop -> Format.fprintf fmt "nop;"
  | Iunimplemented -> Format.fprintf fmt "unimplemented"
