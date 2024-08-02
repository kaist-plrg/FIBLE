open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "wrapper <FILE>"
let ifile = ref ""
let args = ref []
let speclist = [ ("--", Arg.Rest_all (fun x -> args := x), ": arguments") ]

type symbol_type = OBJECT | FUNC

let parse_stab (s : String.t) : (Int64.t * symbol_type * String.t) Option.t =
  let r =
    Str.regexp
      {| *[0-9]+: \([0-9a-f]+\) *[0-9]+ \([A-Z]+\) *[A-Z]+ *[A-Z]+ *[0-9A-Z]+ * *\([^ ]+\)$|}
  in
  if Str.string_match r s 0 then
    let addr = Scanf.sscanf (Str.matched_group 1 s) "%Lx" (fun x -> x) in
    (let* typ =
       match Str.matched_group 2 s with
       | "FUNC" -> Ok FUNC
       | "OBJECT" -> Ok OBJECT
       | _ -> Error "Invalid symbol type"
     in
     let name = Str.matched_group 3 s in
     Ok (addr, typ, name))
    |> Result.to_option
  else None

let main () =
  Arg.parse speclist
    (fun x ->
      if !ifile = "" then ifile := x else raise (Arg.Bad "too many input files"))
    usage_msg;
  if !ifile = "" then (
    Format.printf "no input file\n";
    Arg.usage speclist usage_msg;
    exit 1)
  else ();
  if World.Global.os_type == Linux then (
    let pin, pout =
      Unix.open_process_args "readelf" [| "readelf"; "-a"; "-W"; !ifile |]
    in
    let lines = In_channel.input_lines pin in
    let _ = Unix.close_process (pin, pout) in
    let funcs, objects =
      List.filter_map parse_stab lines
      |> List.partition (fun (_, typ, _) -> typ = FUNC)
    in
    let symtab : Artifact.Data.symbol_table =
      {
        funcs = funcs |> List.map (fun (addr, _, name) -> (addr, name));
        objects = objects |> List.map (fun (addr, _, name) -> (addr, name));
      }
    in
    let data =
      Sexplib.Sexp.to_string_mach (Artifact.Data.sexp_of_symbol_table symtab)
    in
    let ifile_base = Filename.basename !ifile |> Filename.remove_extension in
    let ifile_dirname = Filename.dirname !ifile in
    let dump_path = Filename.concat ifile_dirname (ifile_base ^ ".elf_ext") in
    let binic = In_channel.open_bin !ifile in
    let bindata = In_channel.input_all binic in
    In_channel.close binic;
    let oc = Out_channel.open_bin dump_path in
    Out_channel.output_string oc bindata;
    Out_channel.output_string oc data;
    Out_channel.output_string oc
      (Int64.to_string_le (Int64.of_int (String.length data)));
    Out_channel.output_string oc "ELFX";
    Out_channel.close oc;
    ())
  else Format.printf "Unsupported OS.\n"

let () = Global.run_main main
