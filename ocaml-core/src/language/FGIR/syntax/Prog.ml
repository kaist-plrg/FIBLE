open Sexplib.Std
open Common

type t = {
  funcs : Func.t list;
  rom : DMem.t;
  rspec : Int32.t Int32Map.t;
  externs : String.t Byte8Map.t;
}

let t_of_sexp (se : Sexplib.Type.t) : t =
  match se with
  | List
      [
        List [ Atom "funcs"; funcs ];
        List [ Atom "rom"; rom ];
        List [ Atom "rspec"; rspec ];
        List [ Atom "externs"; externs ];
      ] ->
      {
        funcs = list_of_sexp Func.t_of_sexp funcs;
        rom = DMem.t_of_sexp rom;
        rspec =
          list_of_sexp
            (function
              | List [ k; v ] -> (int32_of_sexp k, int32_of_sexp v)
              | _ -> Sexplib.Conv_error.no_variant_match ())
            rspec
          |> Int32Map.of_list;
        externs =
          list_of_sexp
            (function
              | List [ k; v ] -> (int64_of_sexp k, string_of_sexp v)
              | _ -> Sexplib.Conv_error.no_variant_match ())
            externs
          |> Byte8Map.of_list;
      }
  | _ -> Sexplib.Conv_error.no_variant_match ()

let sexp_of_t (v : t) : Sexplib.Type.t =
  List
    [
      List [ Atom "funcs"; sexp_of_list Func.sexp_of_t v.funcs ];
      List [ Atom "rom"; DMem.sexp_of_t v.rom ];
      List
        [
          Atom "rspec";
          sexp_of_list
            (fun (k, v) -> List [ sexp_of_int32 k; sexp_of_int32 v ])
            (Int32Map.bindings v.rspec);
        ];
      List
        [
          Atom "externs";
          sexp_of_list
            (fun (k, v) -> List [ sexp_of_int64 k; sexp_of_string v ])
            (Byte8Map.bindings v.externs);
        ];
    ]

let get_externs (p : t) : String.t Byte8Map.t = p.externs
let get_rom_byte (p : t) (addr : Byte8.t) : Char.t = DMem.get_byte p.rom addr

let get_rom (p : t) (addr : Byte8.t) (width : Int32.t) : NumericValue.t =
  let rec aux (addr : Byte8.t) (width : Int32.t) (acc : Char.t list) :
      Char.t list =
    if width = 0l then acc
    else
      let c = get_rom_byte p addr in
      aux (Byte8.succ addr) (Int32.pred width) (c :: acc)
  in
  let chars = aux addr width [] |> List.rev in
  NumericValue.of_chars chars

let pp fmt p =
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "funcs: @[<v>%a@]@," (Format.pp_print_list Func.pp) p.funcs;
  Format.fprintf fmt "@]"

let write_prog (p : t) (path : String.t) : unit =
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@.%!" pp p;
  close_out oc

let write_basic_block (p : t) (path : String.t) : unit =
  List.iter
    (fun (f : Func.t) ->
      Func.dump_basic_block f path
        (Option.value f.nameo ~default:(Format.asprintf "%a" Loc.pp f.entry)))
    (List.sort
       (fun (f1 : Func.t) (f2 : Func.t) -> compare f1.entry f2.entry)
       p.funcs)

let get_func_opt (p : t) (loc : Loc.t) : Func.t option =
  List.find_opt (fun (f : Func.t) -> Loc.compare f.entry loc = 0) p.funcs

let from_partial (p : FGIR_partial.Prog.t) : t =
  let funcs = List.map Func.from_partial p.funcs in
  let rom = p.rom in
  let rspec = p.rspec in
  let externs = p.externs in
  { funcs; rom; rspec; externs }
