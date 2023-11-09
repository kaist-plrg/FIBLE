open Basic
open Basic_collection

type time_t = Pre | Post

let time_pp fmt v =
  Format.fprintf fmt "%s" (match v with Pre -> "Pre" | Post -> "Post")

type vertex_t = { block : Block.t; time : time_t }

module Vertex = struct
  type t = vertex_t

  let compare (a : t) (b : t) =
    compare
      (a.block.fLoc, a.block.loc, a.time)
      (b.block.fLoc, b.block.loc, b.time)

  let hash (a : t) = Hashtbl.hash (a.block.fLoc, a.block.loc, a.time)

  let equal (a : t) (b : t) =
    a.block.fLoc = b.block.fLoc && a.block.loc = b.block.loc && a.time = b.time
end

module EdgeLabel = struct
  type t = Inner | Flow

  let compare : t -> t -> int = Stdlib.compare
  let default : t = Flow
end

module G = Graph.Persistent.Digraph.ConcreteLabeled (Vertex) (EdgeLabel)
module Oper = Graph.Oper.P (G)

let to_graph (f : Func.t) : G.t =
  let bbMap : Block.t LocMap.t =
    List.fold_left
      (fun m (b : Block.t) -> LocMap.add b.loc b m)
      LocMap.empty f.blocks
  in
  let g = G.empty in
  let g =
    List.fold_left
      (fun g b ->
        g
        |> Fun.flip G.add_vertex { block = b; time = Pre }
        |> Fun.flip G.add_vertex { block = b; time = Post }
        |> fun g ->
        G.add_edge_e g
          ({ block = b; time = Pre }, Inner, { block = b; time = Post }))
      g f.blocks
  in
  let g =
    LocMap.fold
      (fun _ (b : Block.t) g ->
        List.fold_left
          (fun g (b' : Block.t) ->
            G.add_edge_e g
              ({ block = b; time = Post }, Flow, { block = b'; time = Pre }))
          g
          (Block.succ b
          |> List.filter_map (fun (b : Loc.t) -> LocMap.find_opt b bbMap)))
      bbMap g
  in
  g
