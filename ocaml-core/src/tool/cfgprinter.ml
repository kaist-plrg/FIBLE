open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "cfgprinter <ifile>"
let ifile = ref ""
let cwd = ref ""
let args = ref []
let argv0 = ref ""

let speclist =
  [
    ("-log-path", Arg.String (fun x -> Logger.set_log_file x), ": log path");
    ( "-log-feature",
      Arg.String (fun x -> Logger.add_log_feature x),
      ": add log feature" );
  ]

let create_cfg_per_func (f : FGIR.Syn.Func.t) :
    (Int64.t * Int64.t List.t) List.t =
  (* let prev_map =
       List.fold_left
         (fun acc (b : FGIR.Syn.Block.t) ->
           let succ = FGIR.Syn.Block.succ b |> List.map Loc.get_addr in
           List.fold_left
             (fun acc s ->
               Int64Map.update s
                 (fun x ->
                   match x with
                   | None -> Some [ Loc.get_addr b.loc ]
                   | Some l -> Some (Loc.get_addr b.loc :: l))
                 acc)
             acc succ)
         Int64Map.empty f.blocks
     in
  *)
  List.map
    (fun (b : FGIR.Syn.Block.t) ->
      let loc = Loc.get_addr b.loc in
      let succ = FGIR.Syn.Block.succ b |> List.map Loc.get_addr in
      (* let prev = Int64Map.find_opt loc prev_map |> Option.value ~default:[] in *)
      (loc, succ))
    f.blocks

let create_cfg (p : FGIR.Syn.Prog.t) :
    (Int64.t * Int64.t List.t) List.t Int64Map.t =
  List.map
    (fun (f : FGIR.Syn.Func.t) ->
      (f.entry |> Loc.get_addr, create_cfg_per_func f))
    p.funcs
  |> Int64Map.of_list

let pp_dict (pp_k : Format.formatter -> 'a -> unit)
    (pp_v : Format.formatter -> 'b -> unit) (fmt : Format.formatter)
    (d : ('a * 'b) List.t) : unit =
  Format.fprintf fmt "{";
  Format.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt (k, v) -> Format.fprintf fmt "%a: %a" pp_k k pp_v v))
    d;
  Format.fprintf fmt "}"

let pp_list (pp_v : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (l : 'a List.t) : unit =
  Format.fprintf fmt "[";
  Format.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_v)
    l;
  Format.fprintf fmt "]"

let pp_int64 (fmt : Format.formatter) (i : Int64.t) : unit =
  Format.fprintf fmt "%Ld" i

let print_cfg (cfg : (Int64.t * Int64.t List.t) List.t Int64Map.t) : Unit.t =
  Format.fprintf Format.std_formatter "%a\n%!"
    (pp_dict pp_int64
       (pp_list (fun fmt (a, c) ->
            Format.fprintf fmt "(%a, %a)" pp_int64 a (pp_list pp_int64) c)))
    (Int64Map.bindings cfg)

let main () =
  Arg.parse speclist
    (fun x ->
      if !ifile = "" then ifile := x else raise (Arg.Bad "too many input files"))
    usage_msg;
  if !argv0 = "" then argv0 := !ifile;
  args := !argv0 :: !args;
  if !ifile = "" then raise (Arg.Bad "No input file")
  else
    let data = Artifact.Loader.load !ifile in
    let cfg =
      match data with
      | Artifact.Data.L1 l1 -> create_cfg l1
      | _ -> failwith "Unsupported artifact"
    in
    print_cfg cfg

let () = Global.run_main main
