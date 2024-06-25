open Common
open Basic_domain
open Syn
module VSAnalysis = Graph.ChaoticIteration.Make (ICFG.G) (VSAnalysisDomain)
module WTPO = Graph.WeakTopological.Make (ICFG.G)

let compute_astate_with_graph (rom : DMem.t) (f : Func.t) (g : ICFG.G.t) :
    VSAnalysisDomain.t LocMap.t =
  let wtpo =
    WTPO.recursive_scc g
      { block = Func.get_bb f f.entry |> Option.get; time = Pre }
  in
  let analyze_res =
    VSAnalysis.recurse g wtpo
      (fun v ->
        if v.block.loc = f.entry && v.time = Pre then VSAnalysisDomain.init rom
        else VSAnalysisDomain.bot)
      Graph.ChaoticIteration.FromWto 10
  in
  VSAnalysis.M.fold
    (fun v astate acc ->
      if v.time = Pre then acc else LocMap.add v.block.loc astate acc)
    analyze_res LocMap.empty
