open Common
module Vertex = Loc
module G = Graph.Persistent.Digraph.Concrete (Vertex)

let make_in_degree_map (g : G.t) : int LocMap.t =
  G.fold_vertex
    (fun v' m ->
      List.fold_left
        (fun m v ->
          LocMap.update v (function None -> Some 1 | Some n -> Some (n + 1)) m)
        m (G.succ g v'))
    g LocMap.empty

let in_degree_fast (m : int LocMap.t) (v : Vertex.t) : int =
  match LocMap.find_opt v m with Some n -> n | None -> 0

module Oper = Graph.Oper.P (G)
