open Common

type time_t = Pre | Post
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
  type t = Inner | Flow | Call | Ret

  let compare : t -> t -> int = Stdlib.compare
  let default : t = Flow
end

module G = Graph.Persistent.Digraph.ConcreteLabeled (Vertex) (EdgeLabel)
module Oper = Graph.Oper.P (G)

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
        List.fold_left
          (fun g b ->
            g
            |> Fun.flip G.add_vertex { block = b; time = Pre }
            |> Fun.flip G.add_vertex { block = b; time = Post }
            |> fun g ->
            G.add_edge_e g
              ({ block = b; time = Pre }, Inner, { block = b; time = Post }))
          g f.blocks)
      g p.funcs
  in
  let g =
    LocMap.fold
      (fun _ (bbs : Block.t LocMap.t) g ->
        LocMap.fold
          (fun _ (b : Block.t) g ->
            List.fold_left
              (fun g (b' : Block.t) ->
                G.add_edge_e g
                  ( { block = b; time = Post },
                    (match b.jmp.jmp with JC _ | JT _ -> Call | _ -> Flow),
                    { block = b'; time = Pre } ))
              g
              (match b.jmp.jmp with
              | JC { target = Cdirect { target; _ }; _ }
              | JT { target = Cdirect { target; _ }; _ } ->
                  LocMap.find_opt target bbMap
                  |> Fun.flip Option.bind (fun (bbs : Block.t LocMap.t) ->
                         LocMap.find_opt target bbs)
                  |> Option.to_list
              | JC { target = Cind _; _ } | JT { target = Cind _; _ } -> []
              | JI (Jjump n) | JI (Jfallthrough n) ->
                  LocMap.find_opt n bbs |> Option.to_list
              | JI Junimplemented -> []
              | JR _ -> []
              | JI (Jjump_ind { candidates; _ }) ->
                  LocSet.to_list candidates
                  |> List.map (fun l -> LocMap.find_opt l bbs)
                  |> List.filter_map Fun.id
              | JI (Jcbranch { target_true; target_false; _ }) ->
                  (LocMap.find_opt target_true bbs |> Option.to_list)
                  @ (LocMap.find_opt target_false bbs |> Option.to_list)))
          bbs g)
      bbMap g
  in
  let g =
    LocMap.fold
      (fun _ (f : Func.t) g ->
        let rets = Func.get_ret_blocks f in
        let entry = LocMap.find f.entry (LocMap.find f.entry bbMap) in
        let ret_afters =
          G.pred g { block = entry; time = Pre }
          |> List.filter_map (fun (b : vertex_t) ->
                 match b.block.jmp.jmp with
                 | JC { fallthrough; _ } ->
                     LocMap.find_opt b.block.fLoc bbMap
                     |> Fun.flip Option.bind (fun (bbs : Block.t LocMap.t) ->
                            LocMap.find_opt fallthrough bbs)
                 | _ -> None)
        in
        List.fold_left
          (fun g (b : Block.t) ->
            List.fold_left
              (fun g (b' : Block.t) ->
                G.add_edge_e g
                  ({ block = b; time = Post }, Ret, { block = b'; time = Pre }))
              g ret_afters)
          g rets)
      fMap g
  in
  g
