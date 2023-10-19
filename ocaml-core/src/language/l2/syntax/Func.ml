open Basic
open Basic_collection

type t = {
  nameo : String.t option;
  entry : Loc.t;
  boundaries : LocSet.t;
  sp_boundary : int64 * int64;
  sp_diff : int64;
  blocks : Block.t list;
}

let pp fmt { nameo; entry; boundaries; sp_boundary; sp_diff; blocks } =
  Format.fprintf fmt
    "@[<v 2>name: %a@,\
     entry: %a@,\
     boundaries: %a@,\
    \ sp_boundary: %a@,\
     sp_diff: %Ld@,\n\
    \     blocks: %a@]"
    (Format.pp_print_option Format.pp_print_string)
    nameo Loc.pp entry
    (Format.pp_print_list Loc.pp)
    (LocSet.elements boundaries)
    (fun fmt (x, y) -> Format.fprintf fmt "(%Ld, %Ld)" x y)
    sp_boundary sp_diff
    (Format.pp_print_list Block.pp)
    blocks

let get_bb (f : t) (loc : Loc.t) : Block.t option =
  List.find_opt (fun (b : Block.t) -> compare b.loc loc = 0) f.blocks

let get_preds (f : t) (b : Block.t) : Block.t list =
  List.filter (fun (b' : Block.t) -> List.mem b.loc (Block.succ b')) f.blocks
