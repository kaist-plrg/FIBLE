open StdlibExt
open Notation
open SleighDef

let test_single (s : Sla.t) (a : String.t) : Unit.t =
  let res =
    let* v = Sla.resolve s s.root (ParserWalker.of_mock a) in
    SymbolPrinter.print_constructor v s (ParserWalker.of_mock a)
  in
  match res with
  | Ok s -> Format.printf "%s\n" s
  | Error s -> Format.printf "Error: Sla resolve fail: %s" s

let do_test (s : Sla.t) : Unit.t =
  let tests =
    [
      "\x55";
      "\x48\x89\xe5";
      "\x48\x83\xec\x10";
      (* TODO: "\xc7\x45\xfc\x00\x00\x00\x00";
         "\x83\x45\xfc\x01";
      *)
      "\x8b\x45\xfc";
      "\x89\xc6";
      (* "\xb8\x00\x00\x00\x00" ; *)
      "\x90";
      "\xc9";
      "\xc3";
    ]
  in
  List.iter (test_single s) tests

let test () =
  let ghidra_path = [%pwd] ^ "/ghidra_11.0.3_PUBLIC" in
  let processor =
    ghidra_path ^ "/Ghidra/Processors/x86/data/languages/x86-64.sla"
  in

  let s =
    let* xmlf =
      try Xml.parse_file processor |> Result.ok
      with Xml_light_errors.Xml_error s -> Xml.error s |> Result.error
    in
    Sla.decode xmlf
  in
  match s with
  | Ok s -> do_test s
  | Error s -> Format.printf "Error: Sla parsing fail"
;;

test ()
