open Basic
open Basic_collection
open Common_language

module Inner = struct
  type t = {
    nameo : String.t option;
    entry : Loc.t;
    boundaries : LocSet.t;
    sp_boundary : int64 * int64;
    sp_diff : int64;
    blocks : Block.t list;
  }

  let get_entry t = t.entry
  let get_blocks t = t.blocks

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
end

include Inner
include FuncHelperF.Make (Jmp) (Block) (Inner)
