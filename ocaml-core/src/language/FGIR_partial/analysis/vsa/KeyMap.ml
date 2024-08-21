open Common
open Basic_domain

module Make (A : sig
  include DomainSpec.LatticeWithTop
  include PrettySpec.PrettyPrint with type t := t
end) =
struct
  include TopMapD.MakeLatticeWithTop (Key) (A)

  let find_filter_opt (f : Key.t -> bool) (a : t) : (Key.t * A.t) Option.t =
    fold (fun k v acc -> if f k then Some (k, v) else acc) a None

  let pp (fmt : Format.formatter) (m : t) : unit =
    let pp_pair fmt (k, v) = Format.fprintf fmt "%a -> %a" Key.pp k A.pp v in
    Format.fprintf fmt "{%a}" (Format.pp_print_list pp_pair) (bindings m)

  let keys (a : t) =
    let sk = bindings a |> List.map fst in
    List.sort_uniq Key.compare sk

  let refine_memrefs (a : t) (rs : RegIdFullSet.t) : t =
    to_list a
    |> List.filter_map (fun (k, v) ->
           let k' = Key.refine_memrefs k rs in
           match k' with Some k' -> Some (k', v) | _ -> None)
    |> of_list

  let clear_memref a =
    filter
      (fun (k : Key.t) _ -> match k with KMemLoc _ -> false | _ -> true)
      a

  let clear_tempreg a =
    filter
      (fun (k : Key.t) _ ->
        match k with KReg { id = Unique _; _ } -> false | _ -> true)
      a

  let clear_mr a (r : RegId.t_full) : t =
    to_list a
    |> List.filter_map (fun (k, v) ->
           let k' = Key.clear_mr k r in
           match k' with Some k' -> Some (k', v) | _ -> None)
    |> of_list

  let memory_keys (a : t) : AExprSet.t List.t =
    keys a
    |> List.filter_map (fun (k : Key.t) ->
           match k with KMemLoc v -> Some v | _ -> None)

  let memory_base_regs (a : t) : RegIdFullSet.t =
    memory_keys a
    |> List.fold_left
         (fun (acc : RegIdFullSet.t) (k : AExprSet.t) ->
           RegIdFullSet.union (AExprSet.used_regs k) acc)
         RegIdFullSet.empty

  let join (a : t) (b : t) =
    let amregs = memory_base_regs a in
    let bmregs = memory_base_regs b in
    let inters =
      RegIdFullSet.inter amregs bmregs
      |> RegIdFullSet.elements
      |> List.sort_uniq RegId.compare_full
    in
    match inters with
    | [] -> join (clear_memref a) (clear_memref b)
    | base :: _ ->
        let amreg_rep =
          memory_keys a
          |> List.find (fun k ->
                 AExprSet.exists (fun (x : AExpr.t) -> x.base = base) k)
        in
        let bmreg_rep =
          memory_keys b
          |> List.find (fun k ->
                 AExprSet.exists (fun (x : AExpr.t) -> x.base = base) k)
        in
        let candids =
          RegIdFullSet.union
            (AExprSet.used_regs amreg_rep)
            (AExprSet.used_regs bmreg_rep)
        in
        let final_inters =
          RegIdFullSet.filter
            (fun r -> AExprSet.has_same_diff amreg_rep bmreg_rep base r)
            candids
        in
        join (refine_memrefs a final_inters) (refine_memrefs b final_inters)

  let find_loc_opt (r : AExpr.t) (a : t) : A.t option =
    find_filter_opt
      (fun (k : Key.t) ->
        match k with KMemLoc v -> AExprSet.mem r v | _ -> false)
      a
    |> Option.map snd
end
