open Basic

type t =
  | Iload of (VarNode.t * VarNode.t * VarNode.t)
  | Istore of (VarNode.t * VarNode.t * VarNode.t)
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
  | Iassignment (i, o) ->
      Format.fprintf fmt "%a = %a;" VarNode.pp o Assignable.pp i
  | INop -> Format.fprintf fmt "nop;"
