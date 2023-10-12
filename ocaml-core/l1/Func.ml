open Basic
open Basic_domain

type t = { nameo: String.t option; entry : Loc.t; boundaries : LocSetD.t; blocks : Block.t list }

let pp fmt { nameo; entry; boundaries; blocks } =
  Format.fprintf fmt     "@[<v 2>name: %a@,entry: %a@,boundaries: %a@,blocks: %a@]"
  (Format.pp_print_option Format.pp_print_string) nameo
    Loc.pp
    entry
    (Format.pp_print_list Loc.pp)
    (LocSetD.elements boundaries)
    (Format.pp_print_list Block.pp)
    blocks

let get_bb (f : t) (loc : Loc.t) : Block.t option =
  List.find_opt (fun (b : Block.t) -> compare b.loc loc = 0) f.blocks

let get_preds (f : t) (b : Block.t) : Block.t list =
  List.filter (fun (b' : Block.t) -> List.mem b.loc (Block.succ b')) f.blocks

let get_return_bb (f : t) : Block.t list =
  List.filter
    (fun (b : Block.t) -> match b.jmp with Jret _ -> true | _ -> false)
    f.blocks


let dump_basic_block (f: t) (path: String.t) (filename: String.t): unit =
  let oc = open_out (Filename.concat path (filename ^ ".bb")) in
  let fmt = Format.formatter_of_out_channel oc in
  List.iter (
    fun (b : Block.t) -> Format.fprintf fmt "%a" Block.pp_summary b
    ) (List.sort (fun (b1 : Block.t) (b2 : Block.t) -> compare b1.loc b2.loc) f.blocks);
  Format.fprintf fmt "%!";
  close_out oc