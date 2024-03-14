module Inner = struct
  type t =
    | Iunimplemented
    | Ijump of Loc.t
    | Ijump_ind of VarNode.t
    | Icbranch of { condition : VarNode.t; target : Loc.t }
    | ILS of ILoadStore.t
    | IA of IAssignment.t
    | IN of INop.t

  let pp (fmt : Format.formatter) (p : t) =
    match p with
    | Ijump i -> Format.fprintf fmt "goto %a;" Loc.pp i
    | Ijump_ind i -> Format.fprintf fmt "goto *%a;" VarNode.pp i
    | Icbranch { condition; target } ->
        Format.fprintf fmt "if %a goto %a;" VarNode.pp condition Loc.pp target
    | Iunimplemented -> Format.fprintf fmt "unimplemented"
    | ILS i -> ILoadStore.pp fmt i
    | IA i -> IAssignment.pp fmt i
    | IN i -> INop.pp fmt i

  let is_nop ins = match ins with IN _ -> true | _ -> false
end

include Inner
include InstFullF.Make (Inner)
