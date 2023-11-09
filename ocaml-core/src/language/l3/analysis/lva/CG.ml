open Basic_collection

type cg_vertex_t = Func.t

module CG_vertex = struct
  type t = cg_vertex_t

  let compare (a : t) (b : t) = compare a.entry b.entry
  let hash (a : t) = Hashtbl.hash a.entry
  let equal (a : t) (b : t) = a.entry = b.entry
end

module CG = Graph.Persistent.Digraph.Concrete (CG_vertex)
module CG_Components = Graph.Components.Make (CG)

let to_cg (p : Prog.t) : CG.t =
  let fMap : Func.t LocMap.t =
    List.fold_left
      (fun m (f : Func.t) -> LocMap.add f.entry f m)
      LocMap.empty p.funcs
  in
  let g =
    LocMap.fold
      (fun _ (f : Func.t) g -> g |> Fun.flip CG.add_vertex f)
      fMap CG.empty
  in
  let g =
    LocMap.fold
      (fun _ (f : Func.t) g ->
        List.fold_left
          (fun g (b : Block.t) ->
            match b.jmp.jmp with
            | Jcall (_, t, _) ->
                LocMap.find_opt t fMap
                |> Option.map (fun f' -> CG.add_edge g f f')
                |> Option.value ~default:g
            | _ -> g)
          g f.blocks)
      fMap g
  in
  g
