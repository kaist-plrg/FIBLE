open Basic

module Vertex = struct
  type t = Loc.t

  let compare (a : t) (b : t) = compare a b
  let hash (a : t) = Hashtbl.hash a
  let equal (a : t) (b : t) = a = b
end

module G = Graph.Persistent.Digraph.Concrete (Vertex)
module Oper = Graph.Oper.P (G)
