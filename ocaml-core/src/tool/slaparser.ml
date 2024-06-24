open SleighDef
open World

let usage_msg = "slaparser -i <ifile>"
let ifile = ref ""
let debug = ref false
let entry : Int64.t ref = ref 0L
let disass_only = ref false
let test_dir = ref ""
let inputs : String.t List.t ref = ref []

let speclist =
  [
    ("-i", Arg.Set_string ifile, ": input file");
    ("-t", Arg.Set_string test_dir, ": test spec directory");
    ("-dasm", Arg.Set disass_only, ": disassemble only");
    ("-entry", Arg.String (fun s -> entry := Int64.of_string s), ": entry point");
    ("-d", Arg.Set debug, ": debug");
  ]

let print_tag (xml : Xml.xml) : Unit.t = [%log info "tag: %s" (Xml.tag xml)]

let check (b : Bool.t) : (Unit.t, String.t) Result.t =
  if b then Ok () else Error ""

let print_attribs (xml : Xml.xml) : Unit.t =
  Xml.attribs xml |> List.iter (fun (n, s) -> [%log info "%s: %s" n s])

module StringSet = Set.Make (String)

let do_parse (sla : SleighDef.Sla.t) (input : String.t) (disass_only : Bool.t)
    (entry : Int64.t) : Unit.t =
  [%log debug "scopes: %d" (Int32Map.cardinal sla.symbol_table.scopeMap)];
  [%log debug "symbols: %d" (Int32Map.cardinal sla.symbol_table.symbolMap)];
  [%log
    debug "instruction constructor length: %d"
      (ConstructorMap.cardinal sla.root.construct)];
  let rec aux (offset : Int32.t) : Unit.t =
    if String.length input <= Int32.to_int offset then ()
    else
      let subinput =
        String.sub input (Int32.to_int offset)
          (String.length input - Int32.to_int offset)
      in
      let pc = ParserContext.of_mock subinput in
      let pw = ParserWalker.from_context pc in
      let res =
        let* v = Sla.resolve sla sla.root pw in
        let offset2 = Constructor.calc_length v 0l in
        let pinfo =
          {
            PatternInfo.addr = Int64.add entry (Int64.of_int32 offset);
            naddr = Int64.add entry (Int64.of_int32 (Int32.add offset offset2));
            n2addr = None;
            umask = sla.unique_allocatemask;
            uoffset =
              Int64.logand
                (Int64.add entry (Int64.of_int32 offset))
                (sla.unique_allocatemask |> Int64.of_int32)
              |> Fun.flip Int64.shift_left 4;
          }
        in
        if disass_only then (
          let* s = SymbolPrinter.print_constructor v sla pw pinfo in
          Format.printf "%s\n%!" s;
          offset2 |> Result.ok)
        else
          let* mnem = SymbolPrinter.print_mnem v sla pw pinfo in
          let* v2, _ = Sla.resolve_handle sla v pw pinfo in
          let* s = PCodeBuilder.build_with_mnemonic v2 (-1l) pw pinfo mnem in
          let s =
            if List.length s = 0 then
              [
                {
                  PCode.mnemonic = "NOP";
                  opcode = 999l;
                  inputs = [||];
                  output = None;
                };
              ]
            else s
          in
          let plist =
            List.mapi
              (Translate.pcode_to_common sla.spaceinfo sla.regspec pinfo.addr)
              s
          in

          Format.printf "%a\n%!"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
               Common.RawInst.pp_full)
            plist;
          offset2 |> Result.ok
      in
      match res with
      | Ok offset2 -> aux (Int32.add offset offset2)
      | Error s -> [%log info "%s" s]
  in
  aux 0l

let do_single_file (fname : String.t) (inputs : String.t List.t)
    (disass_only : Bool.t) (entry : Int64.t) : Unit.t =
  let s =
    let* xmlf =
      try Xml.parse_file fname |> Result.ok
      with Xml_light_errors.Xml_error s -> Xml.error s |> Result.error
    in
    Sla.decode xmlf
  in
  inputs
  |> List.iter (fun input ->
         match s with
         | Ok s -> do_parse s input disass_only entry
         | Error s -> [%log info "%s" s])

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

let hex2str (v : String.t) : String.t =
  let len = String.length v in
  let buf = Buffer.create (len / 2) in
  for i = 0 to (len / 2) - 1 do
    let c =
      Char.chr (Scanf.sscanf (String.sub v (i * 2) 2) "%x" (fun x -> x))
    in
    Buffer.add_char buf c
  done;
  Buffer.contents buf

let main () =
  let ghidra_path = [%pwd] ^ "/ghidra_11.0.3_PUBLIC" in
  let processor =
    ghidra_path ^ "/Ghidra/Processors/x86/data/languages/x86-64.sla"
  in
  Arg.parse speclist (fun x -> inputs := hex2str x :: !inputs) usage_msg;
  inputs := List.rev !inputs;
  if !debug then Logger.set_level Logger.Debug;
  match (!ifile, !test_dir) with
  | "", "" -> do_single_file processor !inputs !disass_only !entry
  | fname, "" -> do_single_file fname !inputs !disass_only !entry
  | "", dname -> do_test_dir dname
  | _ -> raise (Arg.Bad "Cannot specify both input file and test directory")

let () = Global.run_main main
