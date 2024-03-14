module Make (Block : sig
  type t

  val get_fLoc : t -> Loc.t
  val get_loc : t -> Loc.t
  val succ : t -> Loc.t list
  val pp : Format.formatter -> t -> unit
end) =
struct
  type time_t = Pre | Post

  let time_pp fmt v =
    Format.fprintf fmt "%s" (match v with Pre -> "Pre" | Post -> "Post")

  type vertex_t = { block : Block.t; time : time_t }

  let pp_vertex fmt (v : vertex_t) =
    Format.fprintf fmt "%a@%a" Block.pp v.block time_pp v.time

  module Vertex = struct
    type t = vertex_t

    let compare (a : t) (b : t) =
      compare
        (Block.get_fLoc a.block, Block.get_loc a.block, a.time)
        (Block.get_fLoc b.block, Block.get_loc b.block, b.time)

    let hash (a : t) =
      Hashtbl.hash (Block.get_fLoc a.block, Block.get_loc a.block, a.time)

    let equal (a : t) (b : t) =
      Block.get_fLoc a.block = Block.get_fLoc b.block
      && Block.get_loc a.block = Block.get_loc b.block
      && a.time = b.time
  end

  module EdgeLabel = struct
    type t = Inner | Flow

    let compare : t -> t -> int = Stdlib.compare
    let default : t = Flow
  end

  module G = Graph.Persistent.Digraph.ConcreteLabeled (Vertex) (EdgeLabel)
  module Oper = Graph.Oper.P (G)

  let to_graph (blist : Block.t List.t) : G.t =
    let bbMap : Block.t LocMap.t =
      List.fold_left
        (fun m (b : Block.t) -> LocMap.add (Block.get_loc b) b m)
        LocMap.empty blist
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
        g blist
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
end
