open Basic
open Basic_collection

type t =
  | Junimplemented
  | Jfallthrough of Loc.t
  | Jjump of Loc.t
  | Jjump_ind of (VarNode.t * LocSet.t)
  | Jcbranch of (VarNode.t * Loc.t * Loc.t)
  | Jcall of (Loc.t * Loc.t)
  | Jcall_ind of (VarNode.t * Loc.t)
  | Jtailcall of Loc.t
  | Jtailcall_ind of VarNode.t
  | Jret of VarNode.t

type t_full = { jmp : t; loc : Loc.t; mnem : Mnemonic.t }

let pp fmt (a : t) =
  match a with
  | Jjump i -> Format.fprintf fmt "goto %a;" Loc.pp i
  | Jjump_ind (i, s) ->
      Format.fprintf fmt "goto *%a (from %a);" VarNode.pp i
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           Loc.pp)
        (LocSet.elements s)
  | Jcbranch (i0, i1, i2) ->
      Format.fprintf fmt "if %a goto %a else goto %a;" VarNode.pp i0 Loc.pp i1
        Loc.pp i2
  | Jfallthrough i -> Format.fprintf fmt "fallthrough %a;" Loc.pp i
  | Junimplemented -> Format.fprintf fmt "unimplemented"
  | Jcall (t, f) -> Format.fprintf fmt "call %a; -> %a" Loc.pp t Loc.pp f
  | Jcall_ind (t, f) ->
      Format.fprintf fmt "call *%a; -> %a" VarNode.pp t Loc.pp f
  | Jtailcall f -> Format.fprintf fmt "tailcall %a;" Loc.pp f
  | Jtailcall_ind f -> Format.fprintf fmt "tailcall *%a;" VarNode.pp f
  | Jret i -> Format.fprintf fmt "return %a;" VarNode.pp i

let succ jmp =
  match jmp with
  | Jcall (_, n) -> [ n ]
  | Jcall_ind (_, n) -> [ n ]
  | Jtailcall _ -> [ ]
  | Jtailcall_ind _ -> [ ]
  | Jcbranch (_, n, m) -> [ n; m ]
  | Jfallthrough n -> [ n ]
  | Jjump n -> [ n ]
  | Jjump_ind (_, s) -> LocSet.to_seq s |> List.of_seq
  | Jret _ -> []
  | Junimplemented -> []

let succ_full jmp = succ jmp.jmp

let pp_full fmt (a : t_full) =
  Format.fprintf fmt "%a: %a [%a]" Loc.pp a.loc pp a.jmp Mnemonic.pp a.mnem
