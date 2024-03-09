open Basic
open Basic_collection

module Inner = struct
  type t = {
    nameo : String.t option;
    entry : Loc.t;
    boundaries : LocSet.t;
    sp_boundary : int64 * int64;
    sp_diff : int64;
    inputs : RegId.t list;
    outputs : RegId.t list;
    blocks : Block.t list;
  }

  let get_blocks { blocks; _ } = blocks
  let get_entry { entry; _ } = entry

  let pp fmt
      {
        nameo;
        entry;
        boundaries;
        sp_boundary;
        sp_diff;
        inputs;
        outputs;
        blocks;
      } =
    Format.fprintf fmt
      "@[<v 2>name: %a@,\
       entry: %a@,\
       boundaries: %a@,\
      \ sp_boundary: %a@,\
       sp_diff: %Ld@,\n\
       inputs: %a@,\
       outputs: %a@,\
      \     blocks: %a@]"
      (Format.pp_print_option Format.pp_print_string)
      nameo Loc.pp entry
      (Format.pp_print_list Loc.pp)
      (LocSet.elements boundaries)
      (fun fmt (x, y) -> Format.fprintf fmt "(%Ld, %Ld)" x y)
      sp_boundary sp_diff
      (Format.pp_print_list RegId.pp)
      inputs
      (Format.pp_print_list RegId.pp)
      outputs
      (Format.pp_print_list Block.pp)
      blocks
end

include Inner
include Common_language.FuncHelperF.Make (Jmp) (Block) (Inner)
