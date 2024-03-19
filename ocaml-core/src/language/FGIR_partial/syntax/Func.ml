open Common
open Basic_domain

module Inner = struct
  type t = {
    nameo : String.t option;
    entry : Loc.t;
    boundaries : LocSetD.t;
    blocks : Block.t list;
  }

  let get_entry (f : t) : Loc.t = f.entry
  let get_blocks (f : t) : Block.t list = f.blocks

  let pp fmt { nameo; entry; boundaries; blocks } =
    Format.fprintf fmt
      "@[<v 2>name: %a@,entry: %a@,boundaries: %a@,blocks: %a@]"
      (Format.pp_print_option Format.pp_print_string)
      nameo Loc.pp entry
      (Format.pp_print_list Loc.pp)
      (LocSetD.elements boundaries)
      (Format.pp_print_list Block.pp)
      blocks
end

include Inner
include FuncHelperF.Make (Jmp) (Block) (Inner)

let dump_basic_block (f : t) (path : String.t) (filename : String.t) : unit =
  let oc = open_out (Filename.concat path (filename ^ ".bb")) in
  let fmt = Format.formatter_of_out_channel oc in
  List.iter
    (fun (b : Block.t) -> Format.fprintf fmt "%Lx\n" (Loc.to_addr b.loc))
    (f.blocks
    |> List.filter (fun (b : Block.t) ->
           (not
              (LocSetD.mem b.loc f.boundaries && List.length (get_preds f b) = 1))
           || b.loc = f.entry)
    |> List.sort (fun (b1 : Block.t) (b2 : Block.t) -> compare b1.loc b2.loc));
  Format.fprintf fmt "%!";
  close_out oc

let find_switchstop_opt (f : t) : Block.t option =
  List.find_opt
    (fun (b : Block.t) ->
      match b.jmp.jmp with JswitchStop _ -> true | _ -> false)
    f.blocks
