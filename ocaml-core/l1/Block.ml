open StdlibExt
open Basic
open Basic_domain

type t = { loc : Loc.t; body : Inst.t list; jmp : Jmp.t }

let fold_left f acc { body; jmp; _ } =
  List.fold_left f acc body

let pp fmt { loc; body; jmp } =
  Format.fprintf fmt "@[<v 2>block %a@ %a@ %a@]" Loc.pp loc
    (Format.pp_print_list Inst.pp)
    body Jmp.pp jmp

let succ { jmp; _ } =
  match jmp with
  | Jmp.Jcall (_, n) -> [ n ]
  | Jmp.Jcall_ind (_, n) -> [ n ]
  | Jmp.Jcbranch (_, n, m) -> [ n; m ]
  | Jmp.Jfallthrough n -> [ n ]
  | Jmp.Jjump n -> [ n ]
  | Jmp.Jjump_ind (_, s) -> LocSetD.to_seq s |> List.of_seq
  | Jmp.Jret _ -> []
  | Jmp.Junimplemented -> []

  let pp_summary fmt { loc; body; jmp } =
    Format.fprintf fmt "Block: %Lx\n" (Loc.to_addr loc);
    Format.fprintf fmt "Successors: %a\n" (Format.pp_print_list Int64Ext.pp ~pp_sep:(fun fmt _ -> Format.fprintf fmt " ")) (succ { loc; body; jmp } |> List.map Loc.to_addr)
  