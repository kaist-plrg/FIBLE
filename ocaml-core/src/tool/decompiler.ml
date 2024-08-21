open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "pcert <ifile>"
let ghidra_path = ref ""
let ifile = ref ""
let dump_path = ref ""
let func_path = ref ""

type dump_flag_type = {
  rom : bool ref;
  cfa : bool ref;
  spfa : bool ref;
  csa : bool ref;
  rea : bool ref;
}

let dump_flag =
  {
    rom = ref true;
    cfa = ref false;
    spfa = ref false;
    csa = ref false;
    rea = ref false;
  }

let cwd = ref ""
let func : String.t Option.t ref = ref None

let speclist =
  [
    ("-debug", Arg.Unit (fun _ -> Logger.set_level Logger.Debug), ": debug mode");
    ("-log-path", Arg.String (fun x -> Logger.set_log_file x), ": log path");
    ( "-log-feature",
      Arg.String (fun x -> Logger.add_log_feature x),
      ": add log feature" );
    ( "-func-name",
      Arg.String (fun x -> func := Some x),
      ": target function name" );
  ]

let dump_cfa (cfa_res : (String.t * Byte8.t * ILIR.Shallow_CFA.t) list)
    (dump_path : string) =
  List.iter
    (fun (fname, _, x) ->
      let dump_cfa_path = Filename.concat dump_path (fname ^ ".boundary") in
      let { ILIR.Shallow_CFA.sound_jump } = x in
      let contained_addrs =
        ILIR.JumpG.G.fold_vertex
          (fun l s -> LocSetD.add l s)
          sound_jump LocSetD.empty
      in
      let oc = open_out dump_cfa_path in
      let ofmt = Format.formatter_of_out_channel oc in
      let sorted_fboundary =
        LocSetD.to_seq contained_addrs
        |> Seq.filter (fun x -> Loc.get_seq x = 0)
        |> Seq.map (fun x -> Loc.get_addr x)
        |> List.of_seq |> List.sort compare
      in
      List.iter (fun x -> Format.fprintf ofmt "%Lx\n" x) sorted_fboundary;
      Format.fprintf ofmt "%!";
      close_out oc)
    cfa_res;
  ()

let dump_spfa (spfa_res : (FGIR.Syn.Func.t * FGIR.SPFA.Immutable.t) list)
    (dump_path : string) =
  List.iter
    (fun ((func : FGIR.Syn.Func.t), x) ->
      let fname : String.t =
        Option.value func.nameo
          ~default:(Format.asprintf "%a" Loc.pp func.entry)
      in
      let dump_spfa_path =
        Filename.concat dump_path (fname ^ ".stack_boundary")
      in
      let { FGIR.SPFA.Immutable.accesses } = x in
      let oc = open_out dump_spfa_path in
      let ofmt = Format.formatter_of_out_channel oc in
      match accesses with
      | FGIR.AccessD.Top -> Format.fprintf ofmt "Top\n"
      | FGIR.AccessD.Fin accesses ->
          (match Int64SetD.min_elt_opt accesses with
          | None -> Format.fprintf ofmt "Empty\n"
          | Some addr -> Format.fprintf ofmt "%Ld\n" addr);
          Format.fprintf ofmt "%!";
          close_out oc)
    spfa_res;
  ()

let classify_input (ifile : String.t) :
    ( Artifact.Data.symbol_table,
      FGIR.Syn.Prog.t,
      ASIR.Syn.Prog.t,
      IOIR.Syn.Prog.t )
    Either4.t
    Option.t =
  try
    match Artifact.Loader.load ifile with
    | Artifact.Data.L1 l1 -> Some (Either4.Second l1)
    | Artifact.Data.L2 l2 -> Some (Either4.Third l2)
    | Artifact.Data.L3 l3 -> Some (Either4.Fourth l3)
  with _ ->
    let ic = In_channel.open_bin ifile in
    let flength = In_channel.length ic in
    let result =
      let* _ = if Int64.compare flength 12L < 0 then Error "" else Ok () in
      In_channel.seek ic (Int64.sub flength 4L);
      let signature = In_channel.really_input_string ic 4 in
      let* _ = match signature with Some "ELFX" -> Ok () | _ -> Error "" in
      In_channel.seek ic (Int64.sub flength 12L);
      let data_length =
        In_channel.really_input_string ic 8
        |> Option.map (fun o -> String.get_int64_le o 0)
      in
      let* data_length =
        match data_length with Some x -> Ok x | None -> Error ""
      in
      In_channel.seek ic (Int64.sub flength (Int64.add 12L data_length));
      let data = In_channel.really_input_string ic (Int64.to_int data_length) in
      let* data = match data with Some x -> Ok x | None -> Error "" in

      try
        let x =
          Artifact.Data.symbol_table_of_sexp (Sexplib.Sexp.of_string data)
        in
        Ok (Either4.First x)
      with _ -> Error ""
    in
    In_channel.close ic;
    result |> Result.to_option

let make_l1 (ifile : String.t) (symtab : Artifact.Data.symbol_table)
    (cwd : String.t) (ifile_base : String.t) : FGIR.Syn.Prog.t =
  let tmp_path = Filename.concat cwd "tmp" in
  if not (Sys.file_exists tmp_path) then Unix.mkdir tmp_path 0o777;
  [%log debug "Input file is %s" ifile];
  let server = Ghidra.make_server ifile !ghidra_path tmp_path cwd in
  let _ = server.dump_rom (!dump_path ^ "/" ^ ifile_base ^ ".dmem") in
  let rom : DMem.t =
    Util.DMemWrapper.read_file (!dump_path ^ "/" ^ ifile_base ^ ".dmem")
  in
  let c0 = Sys.time () in
  let l0 : ILIR.Syn.Prog.t =
    {
      ins_mem = server.instfunc;
      rom;
      rspec = server.regspec.base_size;
      externs = Util.ExternalFunction.to_addrMap server.external_function;
      objects = symtab.objects;
      entries =
        symtab.funcs
        |> List.filter (fun (_, s) ->
               if Option.is_none !func then true
               else Option.equal String.equal !func (Some s));
    }
  in
  let c1 = Sys.time () in
  [%log info "L0 translation time: %f" (c1 -. c0)];
  let cfa_res : (String.t * Byte8.t * ILIR.Shallow_CFA.t) list =
    l0.entries
    |> List.map (fun (e, fname) ->
           (fname, e, ILIR.Shallow_CFA.follow_flow l0 e))
  in
  let c2 = Sys.time () in
  if !(dump_flag.cfa) then dump_cfa cfa_res !dump_path else ();
  [%log info "CFA time: %f" (c2 -. c1)];
  let l1_init : FGIR_partial.Syn.Prog.t =
    FGIR_partial.L0toL1_shallow.translate_prog_from_cfa l0 cfa_res
  in
  let c3 = Sys.time () in
  [%log info "L1 translation time: %f" (c3 -. c2)];
  let l1_refine : FGIR_partial.Syn.Prog.t =
    FGIR_partial.Refine.apply_prog l0 l1_init
  in
  let c4 = Sys.time () in
  [%log info "L1 refinement time: %f" (c4 -. c3)];
  let l1 : FGIR.Syn.Prog.t = l1_refine |> FGIR.Syn.Prog.from_partial in
  let c5 = Sys.time () in
  [%log info "L1 time: %f" (c5 -. c4)];
  let dl1 : Artifact.Data.t = L1 l1 in
  Artifact.Dumper.write_pp dl1 (!dump_path ^ "/" ^ ifile_base ^ ".fgir");
  Artifact.Dumper.dump dl1 (!dump_path ^ "/" ^ ifile_base ^ ".fgir_dump");
  l1

let make_l2 (l1 : FGIR.Syn.Prog.t) (ifile_base : String.t) : ASIR.Syn.Prog.t =
  let c5 = Sys.time () in
  let cnt : int =
    l1.funcs
    |> List.fold_left
         (fun (a : Int64Set.t) (f : FGIR.Syn.Func.t) ->
           List.fold_left
             (fun (a : Int64Set.t) (b : FGIR.Syn.Block.t) ->
               Int64Set.add_seq
                 (List.to_seq
                    (List.map
                       (fun (i : FGIR.Syn.Inst.t_full) -> Loc.get_addr i.loc)
                       b.body))
                 a)
             a f.blocks)
         Int64Set.empty
    |> Int64Set.cardinal
  in
  [%log info "Total number of instructions: %d" cnt];
  let spfa_res : (FGIR.Syn.Func.t * FGIR.SPFA.Immutable.t) list =
    l1.funcs |> List.map (fun x -> (x, FGIR.SPFA.Immutable.analyze x 32l 40l))
  in
  let c6 = Sys.time () in
  [%log info "SPFA time: %f" (c6 -. c5)];
  if !(dump_flag.spfa) then dump_spfa spfa_res !dump_path else ();

  let l2 : ASIR.Syn.Prog.t =
    ASIR.L1toL2.translate_prog_from_spfa l1 spfa_res 32l 40l
  in
  let c7 = Sys.time () in
  [%log info "L2 translation time: %f" (c7 -. c6)];
  let dl2 : Artifact.Data.t = L2 l2 in
  Artifact.Dumper.write_pp dl2 (!dump_path ^ "/" ^ ifile_base ^ ".asir");
  Artifact.Dumper.dump dl2 (!dump_path ^ "/" ^ ifile_base ^ ".asir_dump");
  l2

let make_l3 (l2 : ASIR.Syn.Prog.t) (ifile_base : String.t) : IOIR.Syn.Prog.t =
  let c7 = Sys.time () in
  let lva_res : (ASIR.Syn.Func.t * ASIR.REA.astate) List.t =
    ASIR.REA.compute_all l2
  in
  let c8 = Sys.time () in
  [%log info "REA time: %f" (c8 -. c7)];
  let l3 : IOIR.Syn.Prog.t = IOIR.L2toL3.translate_prog_from_rea l2 lva_res in
  let c9 = Sys.time () in
  [%log info "L3 translation time: %f" (c9 -. c8)];
  (* if !(dump_flag.csa) then
       dump_csa csa_res !dump_path
         (Filename.basename !ifile |> Filename.remove_extension)
     else (); *)
  let dl3 : Artifact.Data.t = L3 l3 in
  Artifact.Dumper.write_pp dl3 (!dump_path ^ "/" ^ ifile_base ^ ".ioir");
  Artifact.Dumper.dump dl3 (!dump_path ^ "/" ^ ifile_base ^ ".ioir_dump");
  l3

let main () =
  let cwd = if String.equal !cwd "" then [%pwd] else !cwd in
  (match Sys.getenv_opt "GHIDRA_PATH" with
  | None -> ghidra_path := cwd ^ "/ghidra_11.0.3_PUBLIC"
  | Some x -> ghidra_path := x);

  Arg.parse speclist
    (fun x ->
      if !ifile = "" then ifile := x else raise (Arg.Bad "too many input files"))
    usage_msg;
  if !ifile = "" then raise (Arg.Bad "No input file")
  else
    let ifile_base = Filename.basename !ifile |> Filename.remove_extension in
    if !dump_path = "" then dump_path := Filename.dirname !ifile;
    let input_data = classify_input !ifile in
    let input_data =
      match input_data with
      | Some x -> x
      | None -> [%log error "Invalid input file"]
    in
    let input_data =
      match input_data with
      | Either4.First symtab ->
          Either3.First (make_l1 !ifile symtab cwd ifile_base)
      | Either4.Second l1 -> Either3.First l1
      | Either4.Third l2 -> Either3.Second l2
      | Either4.Fourth l3 -> Either3.Third l3
    in
    let input_data =
      match input_data with
      | Either3.First l1 -> Either.Left (make_l2 l1 ifile_base)
      | Either3.Second l2 -> Either.Left l2
      | Either3.Third l3 -> Either.Right l3
    in
    let _ =
      match input_data with
      | Either.Left l2 -> make_l3 l2 ifile_base
      | Either.Right l3 -> l3
    in
    ()

let () = Global.run_main main
