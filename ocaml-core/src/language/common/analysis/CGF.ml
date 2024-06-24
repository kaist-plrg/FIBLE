module Make (Func : sig
  type t

  val entry : t -> Loc.t
  val get_call_targets : t -> Loc.t List.t
end) =
struct
  type cg_vertex_t = Func.t

  module CG_vertex = struct
    type t = cg_vertex_t

    let compare (a : t) (b : t) = compare (Func.entry a) (Func.entry b)
    let hash (a : t) = Hashtbl.hash (Func.entry a)
    let equal (a : t) (b : t) = Func.entry a = Func.entry b
  end

  module CG = Graph.Persistent.Digraph.Concrete (CG_vertex)
  module CG_Components = Graph.Components.Make (CG)

  let to_cg (flist : Func.t List.t) : CG.t =
    let fMap : Func.t LocMap.t =
      List.fold_left
        (fun m (f : Func.t) -> LocMap.add (Func.entry f) f m)
        LocMap.empty flist
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
            (fun g (t : Loc.t) ->
              LocMap.find_opt t fMap
              |> Option.map (fun f' -> CG.add_edge g f f')
              |> Option.value ~default:g)
            g (Func.get_call_targets f))
        fMap g
    in
    g
end
