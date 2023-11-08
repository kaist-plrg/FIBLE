open Basic_collection

module G = Graph.Persistent.Digraph.Concrete (struct
  type t = Block.t

  let compare (a : t) (b : t) = compare (a.fLoc, a.loc) (b.fLoc, b.loc)
  let hash (a : t) = Hashtbl.hash (a.fLoc, a.loc)
  let equal (a : t) (b : t) = a.fLoc = b.fLoc && a.loc = b.loc
end)

module Oper = Graph.Oper.P (G)
module WTO = Graph.WeakTopological.Make (G)

let to_graph (p : Prog.t) : G.t =
  let fMap : Func.t LocMap.t =
    List.fold_left
      (fun m (f : Func.t) -> LocMap.add f.entry f m)
      LocMap.empty p.funcs
  in
  let bbMap : Block.t LocMap.t LocMap.t =
    LocMap.fold
      (fun _ (f : Func.t) m ->
        LocMap.add f.entry
          (List.fold_left
             (fun m (b : Block.t) -> LocMap.add b.loc b m)
             LocMap.empty f.blocks)
          m)
      fMap LocMap.empty
  in
  let g = G.empty in
  let g =
    List.fold_left
      (fun g (f : Func.t) ->
        List.fold_left (fun g b -> G.add_vertex g b) g f.blocks)
      g p.funcs
  in
  let g =
    LocMap.fold
      (fun _ (bbs : Block.t LocMap.t) g ->
        LocMap.fold
          (fun _ (b : Block.t) g ->
            List.fold_left
              (fun g (b' : Block.t) -> G.add_edge g b b')
              g
              (match b.jmp.jmp with
              | Jcall (_, t, _) ->
                  LocMap.find_opt t bbMap
                  |> Fun.flip Option.bind (fun (bbs : Block.t LocMap.t) ->
                         LocMap.find_opt t bbs)
                  |> Option.to_list
              | Jcall_ind (_, _, _) -> []
              | Jjump n | Jfallthrough n ->
                  LocMap.find_opt n bbs |> Option.to_list
              | Junimplemented -> []
              | Jret -> []
              | Jjump_ind (_, ls) ->
                  LocSet.to_list ls
                  |> List.map (fun l -> LocMap.find_opt l bbs)
                  |> List.filter_map Fun.id
              | Jcbranch (_, lt, lf) ->
                  (LocMap.find_opt lt bbs |> Option.to_list)
                  @ (LocMap.find_opt lf bbs |> Option.to_list)))
          bbs g)
      bbMap g
  in
  let g =
    LocMap.fold
      (fun _ (f : Func.t) g ->
        let rets = Func.get_ret_blocks f in
        let entry = LocMap.find f.entry (LocMap.find f.entry bbMap) in
        let ret_afters =
          G.pred g entry
          |> List.filter_map (fun (b : Block.t) ->
                 match b.jmp.jmp with
                 | Jcall (_, _, r) | Jcall_ind (_, _, r) ->
                     LocMap.find_opt b.fLoc bbMap
                     |> Fun.flip Option.bind (fun (bbs : Block.t LocMap.t) ->
                            LocMap.find_opt b.loc bbs)
                 | _ -> None)
        in
        List.fold_left
          (fun g (b : Block.t) ->
            List.fold_left
              (fun g (b' : Block.t) -> G.add_edge g b b')
              g ret_afters)
          g rets)
      fMap g
  in
  g

module LiveVariableDomain = struct
  type t = Unit.t

  let join (a : t) (b : t) = ()
  let widening (a : t) (b : t) = ()
  let le (prev : t) (generated : t) : bool = true
  let analyze (e : G.E.t) (a : t) : t = ()
end

module LiveVariableAnalysis =
  Graph.ChaoticIteration.Make
    (G)
    (struct
      type t = LiveVariableDomain.t
      type edge = G.E.t
      type vertex = G.V.t
      type g = G.t

      let join = LiveVariableDomain.join
      let widening = LiveVariableDomain.widening
      let equal a b = LiveVariableDomain.le b a && LiveVariableDomain.le a b
      let analyze = LiveVariableDomain.analyze
    end)

let analysis (p : Prog.t) =
  let init_data b = () in
  let pgraph = to_graph p in
  let pmirror = Oper.mirror pgraph in
  LiveVariableAnalysis.recurse pmirror
    (WTO.recursive_scc pmirror
       (p.funcs |> List.hd |> (fun x -> x.blocks) |> List.hd))
    init_data FromWto 0
