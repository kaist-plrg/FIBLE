open StdlibExt
open SleighDef
open World
open Notation

let usage_msg = "slaparser -i <ifile>"
let ifile = ref ""
let test_dir = ref ""

let speclist =
  [
    ("-i", Arg.Set_string ifile, ": input file");
    ("-t", Arg.Set_string test_dir, ": test spec directory");
  ]

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
    [%log info "a"];
    let* v = Sla.resolve s s.root (ParserWalker.of_mock "\x89\xe5") in
    [%log info "resolve"];
    let* s =
      SymbolPrinter.print_constructor v s (ParserWalker.of_mock "\x89\xe5")
    in
    [%log info "resolve: %s" s] |> Result.ok
  in
  match res with Ok () -> () | Error s -> [%log info "%s" s]

let do_single_file (fname : String.t) : Unit.t =
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

let do_test_dir (dname : String.t) : Unit.t =
  let processor_dir = dname ^ "/Ghidra/Processors" in
  let dirs = Sys.readdir processor_dir |> Array.to_list in
  let dirs = List.filter (fun d -> d.[0] <> '.') dirs in
  let dirs =
    List.map (fun d -> processor_dir ^ "/" ^ d ^ "/data/languages") dirs
  in
  let testspecs =
    List.map
      (fun d ->
        (try Sys.readdir d |> Array.to_list with Sys_error _ -> [])
        |> List.filter_map (fun f ->
               if Filename.check_suffix f ".sla" then Some (d ^ "/" ^ f)
               else None))
      dirs
    |> List.concat
  in
  List.iter
    (fun f ->
      let s =
        let* xmlf =
          try Xml.parse_file f |> Result.ok
          with Xml_light_errors.Xml_error s -> Xml.error s |> Result.error
        in
        Sla.decode xmlf
      in
      match s with
      | Ok s -> [%log info "loaded %s" f]
      | Error s -> [%log info "failed to load %s" f])
    testspecs

let main () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage_msg;
  match (!ifile, !test_dir) with
  | "", "" -> raise (Arg.Bad "No input file")
  | fname, "" -> do_single_file fname
  | "", dname -> do_test_dir dname
  | _ -> raise (Arg.Bad "Cannot specify both input file and test directory")

let () = Global.run_main main
