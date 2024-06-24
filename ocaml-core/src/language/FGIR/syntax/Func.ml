open Common
open Basic_domain

module Inner = struct
  type t = {
    nameo : String.t Option.t;
    entry : Loc.t;
    boundaries : LocSetD.t;
    blocks : Block.t List.t;
  }
  [@@deriving sexp, show, fields]
end

include Inner
include FuncHelperF.Make (Jmp) (Block) (Inner)

let dump_basic_block (f : t) (path : String.t) (filename : String.t) : unit =
  let oc = open_out (Filename.concat path (filename ^ ".bb")) in
  let fmt = Format.formatter_of_out_channel oc in
  List.iter
    (fun (b : Block.t) -> Format.fprintf fmt "%Lx\n" (Loc.get_addr b.loc))
    (f.blocks
    |> List.filter (fun (b : Block.t) ->
           (not
              (LocSetD.mem b.loc f.boundaries && List.length (get_preds f b) = 1))
           || b.loc = f.entry)
    |> List.sort (fun (b1 : Block.t) (b2 : Block.t) -> compare b1.loc b2.loc));
  Format.fprintf fmt "%!";
  close_out oc

let from_partial (p : FGIR_partial.Func.t) : t =
  let entry = p.entry in
  let boundaries = p.boundaries in
  let blocks = List.map Block.from_partial p.blocks in
  let nameo = p.nameo in
  { nameo; entry; boundaries; blocks }
