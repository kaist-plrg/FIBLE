open Basic
open Basic_domain

type t =
  | Junimplemented
  | Jfallthrough of Loc.t
  | Jjump of Loc.t
  | Jjump_ind of (VarNode.t * LocSetD.t)
  | Jcbranch of (VarNode.t * Loc.t * Loc.t)
  | Jcall of (Loc.t * Loc.t)
  | Jcall_ind of (VarNode.t * Loc.t)
  | Jret of VarNode.t

let pp fmt (a : t) =
  match a with
  | Jjump i -> Format.fprintf fmt "goto %a;" Loc.pp i
  | Jjump_ind (i, s) ->
      Format.fprintf fmt "goto *%a (from %a);" VarNode.pp i
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           Loc.pp)
        (LocSetD.elements s)
  | Jcbranch (i0, i1, i2) ->
      Format.fprintf fmt "if %a goto %a else goto %a;" VarNode.pp i0 Loc.pp i1
        Loc.pp i2
  | Jfallthrough i -> Format.fprintf fmt "fallthrough %a;" Loc.pp i
  | Junimplemented -> Format.fprintf fmt "unimplemented"
  | Jcall (t, f) -> Format.fprintf fmt "call %a; -> %a" Loc.pp t Loc.pp f
  | Jcall_ind (t, f) ->
      Format.fprintf fmt "call *%a; -> %a" VarNode.pp t Loc.pp f
  | Jret i -> Format.fprintf fmt "return %a;" VarNode.pp i
