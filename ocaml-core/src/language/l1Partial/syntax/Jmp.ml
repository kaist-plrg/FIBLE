open Basic
open Basic_collection
open Common_language

module Inner = struct
  type t =
    | Junimplemented
    | JswitchStop of VarNode.t
    | Jfallthrough of Loc.t
    | Jjump of Loc.t
    | Jjump_ind of { target : VarNode.t; candidates : LocSet.t; sound : Bool.t }
    | Jcbranch of {
        condition : VarNode.t;
        target_true : Loc.t;
        target_false : Loc.t;
      }
    | Jcall of { target : Loc.t; fallthrough : Loc.t }
    | Jcall_ind of { target : VarNode.t; fallthrough : Loc.t }
    | Jtailcall of Loc.t
    | Jtailcall_ind of VarNode.t
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
    | JswitchStop vn -> Format.fprintf fmt "switch stop %a;" VarNode.pp vn
    | Jcall { target; fallthrough } ->
        Format.fprintf fmt "call %a; -> %a" Loc.pp target Loc.pp fallthrough
    | Jcall_ind { target; fallthrough } ->
        Format.fprintf fmt "call *%a; -> %a" VarNode.pp target Loc.pp
          fallthrough
    | Jtailcall f -> Format.fprintf fmt "tailcall %a;" Loc.pp f
    | Jtailcall_ind f -> Format.fprintf fmt "tailcall *%a;" VarNode.pp f
    | Jret i -> Format.fprintf fmt "return %a;" VarNode.pp i

  let succ jmp =
    match jmp with
    | Jcall { fallthrough; _ } -> [ fallthrough ]
    | Jcall_ind { fallthrough; _ } -> [ fallthrough ]
    | Jtailcall _ -> []
    | Jtailcall_ind _ -> []
    | Jcbranch { target_true; target_false; _ } -> [ target_true; target_false ]
    | Jfallthrough n -> [ n ]
    | Jjump n -> [ n ]
    | Jjump_ind { candidates; _ } -> LocSet.to_seq candidates |> List.of_seq
    | Jret _ -> []
    | Junimplemented -> []
    | JswitchStop _ -> []

  let is_ret jmp = match jmp with Jret _ -> true | _ -> false
end

include Inner
include JmpFullF.Make (Inner)
