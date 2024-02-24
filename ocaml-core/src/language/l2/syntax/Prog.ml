open StdlibExt
open Basic
open Basic_collection

type t = {
  sp_num : Int32.t;
  funcs : Func.t list;
  rom : ROM.t;
  rspec : Int32.t Int32Map.t;
  externs : String.t AddrMap.t;
}

let get_rom_byte (p : t) (addr : Addr.t) : Char.t = ROM.get_byte p.rom addr

let get_rom (p : t) (addr : Addr.t) (width : Int32.t) :
    Common_language.NumericValue.t =
  let rec aux (addr : Addr.t) (width : Int32.t) (acc : Char.t list) :
      Char.t list =
    if width = 0l then acc
    else
      let c = get_rom_byte p addr in
      aux (Addr.succ addr) (Int32.pred width) (c :: acc)
  in
  let chars = aux addr width [] |> List.rev in
  Common_language.NumericValue.of_chars chars

let pp fmt p =
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "funcs: @[<v>%a@]@," (Format.pp_print_list Func.pp) p.funcs;
  Format.fprintf fmt "@]"

let write_prog (p : t) (path : String.t) (filename : String.t) : unit =
  let oc = open_out (Filename.concat path (filename ^ ".asir")) in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a%!" pp p;
  close_out oc

let dump_prog (p : t) (path : String.t) (filename : String.t) : unit =
  let oc = open_out_bin (Filename.concat path (filename ^ ".asir_dump")) in
  Marshal.to_channel oc p [];
  close_out oc

let load_prog (filename : String.t) : t =
  let ic = open_in (filename ^ ".asir_dump") in
  let p = Marshal.from_channel ic in
  close_in ic;
  p

let get_func_opt (p : t) (loc : Loc.t) : Func.t option =
  List.find_opt (fun (f : Func.t) -> Loc.compare f.entry loc = 0) p.funcs
