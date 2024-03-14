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

let pp fmt (v : t) =
  match v with
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

let succ (v : t) : Loc.t List.t =
  match v with
  | Jcbranch { target_true; target_false; _ } -> [ target_true; target_false ]
  | Jfallthrough n -> [ n ]
  | Jjump n -> [ n ]
  | Jjump_ind { candidates; _ } -> LocSet.to_seq candidates |> List.of_seq
  | Junimplemented -> []

let get_call_target (v : t) = None
let is_ret (v : t) = false
