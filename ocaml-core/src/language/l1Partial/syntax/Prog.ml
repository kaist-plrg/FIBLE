open StdlibExt
open Basic
open Basic_collection
open Common_language

type t = {
  funcs : Func.t list;
  rom : ROM.t;
  rspec : Int32.t Int32Map.t;
  externs : String.t AddrMap.t;
}

let get_rom_byte (p : t) (addr : Addr.t) : Char.t = ROM.get_byte p.rom addr

let get_rom_raw (rom : ROM.t) (addr : Addr.t) (width : Int32.t) :
    Common_language.NumericValue.t =
  let rec aux (addr : Addr.t) (width : Int32.t) (acc : Char.t list) :
      Char.t list =
    if width = 0l then acc
    else
      let c = ROM.get_byte rom addr in
      aux (Addr.succ addr) (Int32.pred width) (c :: acc)
  in
  let chars = aux addr width [] |> List.rev in
  Common_language.NumericValue.of_chars chars

let get_rom (p : t) (addr : Addr.t) (width : Int32.t) :
    Common_language.NumericValue.t =
  get_rom_raw p.rom addr width

let pp fmt p =
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "funcs: @[<v>%a@]@," (Format.pp_print_list Func.pp) p.funcs;
  Format.fprintf fmt "@]"

let dump_prog (p : t) (path : String.t) (filename : String.t) : unit =
  let oc = open_out (Filename.concat path (filename ^ ".l1_partial")) in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@.%!" pp p;
  close_out oc

let dump_basic_block (p : t) (path : String.t) (filename : String.t) : unit =
  List.iter
    (fun (f : Func.t) ->
      Func.dump_basic_block f path
        (Option.value f.nameo ~default:(Format.asprintf "%a" Loc.pp f.entry)))
    (List.sort
       (fun (f1 : Func.t) (f2 : Func.t) -> compare f1.entry f2.entry)
       p.funcs)

let get_func_opt (p : t) (loc : Loc.t) : Func.t option =
  List.find_opt (fun (f : Func.t) -> Loc.compare f.entry loc = 0) p.funcs
