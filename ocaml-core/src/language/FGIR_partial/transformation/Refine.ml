open StdlibExt
open Common
module PC = Graph.Path.Check (ICFG.G)

let gen_dep_locs (pdg : PDG.ALocSet.t LocMap.t) (init_locs : Loc.t List.t) :
    LocSet.t =
  let rec aux (s : LocSet.t) (wlist : Loc.t List.t) : LocSet.t =
    match wlist with
    | [] -> s
    | x :: xs ->
        if LocSet.mem x s then aux s xs
        else
          let s' = LocSet.add x s in
          let xs' =
            LocMap.find_opt x pdg
            |> Option.map (fun (s : PDG.ALocSet.t) ->
                   PDG.ALocSet.elements s
                   |> List.filter_map (fun (a : PDG.ALoc.t) ->
                          match a with
                          | PDG.ALoc.ALoc l -> Some l
                          | PDG.ALoc.AStart -> None))
            |> Option.value ~default:[] |> List.append xs
          in
          aux s' xs'
  in
  aux LocSet.empty init_locs

let anonymize (jmp : Jmp.t_full) : Jmp.t_full =
  { jmp with jmp = JI Junimplemented }

let extract_loc (f : Func.t) (locs : LocSet.t) : Func.t =
  let blocks =
    List.map
      (fun (b : Block.t) ->
        let body =
          List.fold_left
            (fun insts (s : Inst.t_full) ->
              if LocSet.mem s.loc locs then s :: insts else insts)
            [] b.body
        in
        let jmp =
          if LocSet.mem b.jmp.loc locs then b.jmp else anonymize b.jmp
        in
        { b with body = List.rev body; jmp })
      f.blocks
  in
  { f with blocks }

let extract_dep_loc (f : Func.t) (b : Block.t) : Func.t =
  let g = ICFG.to_graph f.blocks in
  let pdg = PDG.compute_ud_func_with_graph f g in
  let checker = PC.create g in
  let init_locs =
    List.fold_left
      (fun s (x : Block.t) ->
        if
          PC.check_path checker { block = x; time = Pre }
            { block = b; time = Pre }
        then LocSet.add x.jmp.loc s
        else s)
      (LocSet.singleton b.jmp.loc)
      f.blocks
    |> LocSet.elements
  in
  let dep_locs = gen_dep_locs pdg init_locs in
  extract_loc f dep_locs

let do_analysis (rom : DMem.t) (f : Func.t) (b : Block.t) : Int64Set.t option =
  let g = ICFG.to_graph f.blocks in
  let vsa = VSA.compute_astate_with_graph rom f g in
  [%log debug "VSA: %a" VSAnalysisDomain.pp (LocMap.find f.entry vsa)];
  [%log debug "VSA: %a" VSAnalysisDomain.pp (LocMap.find b.loc vsa)];
  match (b.jmp.jmp, LocMap.find_opt b.loc vsa) with
  | JswitchStop v, Some (VSAnalysisDomain.LV (a, _)) ->
      VSAnalysisDomain.Lattice_noBot.try_concretize_vn a v
  | _ -> None

let apply (p0 : ILIR.Prog.t) (f : Func.t) : Func.t =
  let rec aux (f : Func.t) (known_addrs : LocSet.t LocMap.t) : Func.t =
    let b = Func.find_switchstop_opt f in
    match b with
    | None -> f
    | Some b ->
        let x = extract_dep_loc f b in
        let addrs = do_analysis p0.rom x b in

        let new_known_addrs =
          match addrs with
          | None -> LocMap.add b.jmp.loc LocSet.empty known_addrs
          | Some addrs ->
              LocMap.add b.jmp.loc
                (addrs |> Int64Set.to_list
                |> List.map (fun (a : Addr.t) -> (a, 0))
                |> LocSet.of_list)
                known_addrs
        in
        let ncfa =
          ILIR.Shallow_CFA.follow_flow_with_known_addr p0 (Loc.to_addr f.entry)
            new_known_addrs
        in
        let new_f =
          L0toL1_shallow.translate_func p0 f.nameo (Loc.to_addr f.entry) ncfa
            new_known_addrs
        in
        aux new_f new_known_addrs
  in
  aux f LocMap.empty

let apply_prog (p0 : ILIR.Prog.t) (p : Prog.t) : Prog.t =
  let funcs = p.funcs in
  let funcs' = List.map (apply p0) funcs in
  { p with funcs = funcs' }
