open StdlibExt
open Basic

type t = { funcs : Func.t list; rom : Addr.t -> int64 }

let get_rom (p : t) (addr : Addr.t) (width : int32) : int64 =
  let v_full = p.rom addr in
  Int64Ext.cut_width v_full width

let pp fmt p =
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "funcs: @[<v>%a@]@," (Format.pp_print_list Func.pp) p.funcs;
  Format.fprintf fmt "@]"

let dump_prog (p : t) (path : String.t) (filename : String.t) : unit =
  let oc = open_out (Filename.concat path (filename ^ ".l1")) in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@.%!" pp p;
  close_out oc

let dump_basic_block (p : t) (path : String.t) (filename : String.t) : unit =
  List.iter (fun (f: Func.t) -> Func.dump_basic_block f path (Option.value f.nameo ~default:(Format.asprintf "%a" Loc.pp f.entry))) (List.sort (fun (f1: Func.t) (f2: Func.t) -> compare f1.entry f2.entry) p.funcs)
