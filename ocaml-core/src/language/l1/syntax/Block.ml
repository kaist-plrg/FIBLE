open StdlibExt
open Basic
open Basic_domain

type t = { loc : Loc.t; body : Inst.t_full list; jmp : Jmp.t_full }

let fold_left f acc { body; jmp; _ } = List.fold_left f acc body

let pp fmt { loc; body; jmp } =
  Format.fprintf fmt "@[<v 2>block %a@ %a@ %a@]" Loc.pp loc
    (Format.pp_print_list Inst.pp_full)
    body Jmp.pp_full jmp

let succ { jmp; _ } =
  match jmp.jmp with
  | Jmp.Jcall (_, n) -> [ n ]
  | Jmp.Jcall_ind (_, n) -> [ n ]
  | Jmp.Jcbranch (_, n, m) -> [ n; m ]
  | Jmp.Jfallthrough n -> [ n ]
  | Jmp.Jjump n -> [ n ]
  | Jmp.Jjump_ind (_, s) -> LocSetD.to_seq s |> List.of_seq
  | Jmp.Jret _ -> []
  | Jmp.Junimplemented -> []
