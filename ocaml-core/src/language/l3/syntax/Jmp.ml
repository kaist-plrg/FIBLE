open Basic
open Basic_collection

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
        outputs : RegId.t List.t;
        inputs : VarNode.t List.t;
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
        returns : VarNode.t List.t;
        outputs : RegId.t List.t;
        inputs : VarNode.t List.t;
        target : Loc.t;
      }
    | Jtailcall_ind of {
        reserved_stack : Int64.t;
        sp_diff : Int64.t;
        returns : VarNode.t List.t;
        target : VarNode.t;
      }
    | Jret of VarNode.t List.t

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
    | Jcall { reserved_stack; sp_diff; outputs; inputs; target; fallthrough } ->
        Format.fprintf fmt "%a = call (+%Lx) %a(%a : [+%Lx]); -> %a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             RegId.pp)
          outputs sp_diff Loc.pp target
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             VarNode.pp)
          inputs reserved_stack Loc.pp fallthrough
    | Jcall_ind { reserved_stack; sp_diff; target; fallthrough } ->
        Format.fprintf fmt "all = call_ind (+%Lx) *%a(all : [+%Lx]); -> %a"
          sp_diff VarNode.pp target reserved_stack Loc.pp fallthrough
    | Jtailcall { reserved_stack; sp_diff; returns; outputs; inputs; target } ->
        Format.fprintf fmt "%a = tailcall (+%Lx) %a(%a : [+%Lx]) -> ret %a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             RegId.pp)
          outputs sp_diff Loc.pp target
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             VarNode.pp)
          inputs reserved_stack
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             VarNode.pp)
          returns
    | Jtailcall_ind { reserved_stack; sp_diff; returns; target } ->
        Format.fprintf fmt
          "all = tailcall_ind (+%Lx) %a(all : [+%Lx]) -> ret %a" sp_diff
          VarNode.pp target reserved_stack
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             VarNode.pp)
          returns
    | Jret outputs ->
        Format.fprintf fmt "return %a;"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             VarNode.pp)
          outputs

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
include Common_language.JmpFullF.Make (Inner)
