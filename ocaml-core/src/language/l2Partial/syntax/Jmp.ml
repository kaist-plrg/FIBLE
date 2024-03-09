open Basic
open Basic_collection
open Common_language

module Inner = struct
  type t =
    | Junimplemented
    | Jfallthrough of Loc.t
    | Jjump of Loc.t
    | Jjump_ind of { target : VarNode.t; candidates : LocSet.t; sound : Bool.t }
    | Jcbranch of {
        condition : VarNode.t;
        target_true : Loc.t;
        target_false : Loc.t;
      }
    | Jcall of {
        reserved_stack : Int64.t;
        sp_diff : Int64.t;
        target : Loc.t;
        fallthrough : Loc.t;
      }
    | Jcall_ind of {
        reserved_stack : Int64.t;
        sp_diff : Int64.t;
        target : VarNode.t;
        fallthrough : Loc.t;
      }
    | Jtailcall of {
        reserved_stack : Int64.t;
        sp_diff : Int64.t;
        target : Loc.t;
      }
    | Jtailcall_ind of {
        reserved_stack : Int64.t;
        sp_diff : Int64.t;
        target : VarNode.t;
      }
    | Jret of VarNode.t

  let pp fmt (a : t) =
    match a with
    | Jjump i -> Format.fprintf fmt "goto %a;" Loc.pp i
    | Jjump_ind { target; candidates; _ } ->
        Format.fprintf fmt "goto *%a (from %a);" VarNode.pp target
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             Loc.pp)
          (LocSet.elements candidates)
    | Jcbranch { condition; target_true; target_false } ->
        Format.fprintf fmt "if %a goto %a else goto %a;" VarNode.pp condition
          Loc.pp target_true Loc.pp target_false
    | Jfallthrough i -> Format.fprintf fmt "fallthrough %a;" Loc.pp i
    | Junimplemented -> Format.fprintf fmt "unimplemented"
    | Jcall { reserved_stack; sp_diff; target; fallthrough } ->
        Format.fprintf fmt "call (+%Lx) %a[%Lx]; -> %a" sp_diff Loc.pp target
          reserved_stack Loc.pp fallthrough
    | Jcall_ind { reserved_stack; sp_diff; target; fallthrough } ->
        Format.fprintf fmt "call (+%Lx) *%a[%Lx]; -> %a" sp_diff VarNode.pp
          target reserved_stack Loc.pp fallthrough
    | Jtailcall { reserved_stack; sp_diff; target } ->
        Format.fprintf fmt "tailcall (+%Lx) %a[%Lx];" sp_diff Loc.pp target
          reserved_stack
    | Jtailcall_ind { reserved_stack; sp_diff; target } ->
        Format.fprintf fmt "tailcall (+%Lx) *%a[%Lx];" sp_diff VarNode.pp target
          reserved_stack
    | Jret i -> Format.fprintf fmt "return %a;" VarNode.pp i

  let succ jmp =
    match jmp with
    | Jcall { fallthrough; _ }
    | Jcall_ind { fallthrough; _ }
    | Jfallthrough fallthrough ->
        [ fallthrough ]
    | Jtailcall _ | Jtailcall_ind _ | Jret _ | Junimplemented -> []
    | Jcbranch { target_true; target_false } -> [ target_true; target_false ]
    | Jjump n -> [ n ]
    | Jjump_ind { candidates; _ } -> LocSet.to_seq candidates |> List.of_seq

  let is_ret jmp = match jmp with Jret _ -> true | _ -> false

  let get_call_target (j : t) : Loc.t option =
    match j with
    | Jcall { target; _ } -> Some target
    | Jtailcall { target; _ } -> Some target
    | _ -> None
end

include Inner
include JmpFullF.Make (Inner)
