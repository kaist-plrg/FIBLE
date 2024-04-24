open StdlibExt
open SleighDef
open World
open Notation

let usage_msg = "slaparser -i <ifile>"
let ifile = ref ""
let speclist = [ ("-i", Arg.Set_string ifile, ": input file") ]
let print_tag (xml : Xml.xml) : Unit.t = [%log info "tag: %s" (Xml.tag xml)]

let check (b : Bool.t) : (Unit.t, String.t) Result.t =
  if b then Ok () else Error ""

let print_attribs (xml : Xml.xml) : Unit.t =
  Xml.attribs xml |> List.iter (fun (n, s) -> [%log info "%s: %s" n s])

module StringSet = Set.Make (String)

let do_a (s : SleighDef.Sla.t) : Unit.t =
  let res =
    [%log debug "scopes: %d" (Int32Map.cardinal s.symbol_table.scopeMap)];
    [%log debug "symbols: %d" (Int32Map.cardinal s.symbol_table.symbolMap)];
    [%log
      debug "instruction constructor length: %d"
        (ConstructorMap.cardinal s.root.construct)];
    let* v =
      SubtableSymbol.resolve s.root (ParserWalker.of_mock "\xc2\x13\x00")
    in
    [%log info "resolve: %a" Constructor.pp_printpiece v] |> Result.ok
  in
  match res with Ok () -> () | Error s -> [%log info "%s" s]

let main () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage_msg;
  if !ifile = "" then raise (Arg.Bad "No input file")
  else
    let s =
      let* xmlf =
        try Xml.parse_file !ifile |> Result.ok
        with Xml_light_errors.Xml_error s -> Xml.error s |> Result.error
      in
      Sla.decode xmlf
    in
    match s with Ok s -> do_a s | Error s -> [%log info "%s" s]

let () = Global.run_main main
