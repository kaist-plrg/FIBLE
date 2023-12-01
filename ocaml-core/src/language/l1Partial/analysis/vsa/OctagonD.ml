open Basic
open Basic_collection
open Basic_domain
open Value_domain

module UpperD = struct
  type t = Bounded of Int64.t | Top | Bot

  let compare = compare
  let top = Top
  let map f a = match a with Bounded i -> Bounded (f i) | _ -> a

  let add a b =
    match (a, b) with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Bounded a, Bounded b -> Bounded (Int64.add a b)
    | _, _ -> Top

  let join a b =
    match (a, b) with
    | Bot, _ -> b
    | _, Bot -> a
    | Bounded a, Bounded b ->
        if a = b then Bounded a else Bounded (Int64.max a b)
    | _, _ -> Top

  let meet a b =
    match (a, b) with
    | Top, _ -> b
    | _, Top -> a
    | Bounded a, Bounded b ->
        if a = b then Bounded a else Bounded (Int64.min a b)
    | _, _ -> Bot

  let le a b =
    match (a, b) with
    | Bot, _ -> true
    | _, Bot -> false
    | Bounded a, Bounded b -> a <= b
    | Top, Bounded _ -> false
    | _, _ -> true

  let pp fmt a =
    match a with
    | Bounded i -> Format.fprintf fmt "%Ld" i
    | Top -> Format.fprintf fmt "Top"
    | Bot -> Format.fprintf fmt "Bot"
end

module Map = struct
  include TopMapD.MakeLatticeWithTop (KeyPair) (UpperD)

  let find_filter_opt (f : KeyPair.t -> bool) (a : t) :
      (KeyPair.t * UpperD.t) Option.t =
    fold (fun k v acc -> if f k then Some (k, v) else acc) a None

  let find_loc_opt_1 (a : t) (r1 : AExpr.t) (r2 : RegId.t) : UpperD.t option =
    find_filter_opt
      (fun (k1, k2) ->
        match (k1, k2) with
        | KMemLoc v, KReg r -> AExprSet.mem r1 v && r = r2
        | _ -> false)
      a
    |> Option.map snd

  let find_loc_opt_2 (a : t) (r1 : RegId.t) (r2 : AExpr.t) : UpperD.t option =
    find_filter_opt
      (fun (k1, k2) ->
        match (k1, k2) with
        | KReg r, KMemLoc v -> r = r1 && AExprSet.mem r2 v
        | _ -> false)
      a
    |> Option.map snd
end

type t = Map.t

let clear_memref a =
  Map.filter
    (fun k _ ->
      match k with KMemLoc _, _ -> false | _, KMemLoc _ -> false | _ -> true)
    a

let clear_mr a (r : RegId.t) =
  Map.to_list a
  |> List.filter_map (fun ((k1, k2), v) ->
         let k1' = Key.clear_mr k1 r in
         let k2' = Key.clear_mr k2 r in
         match (k1', k2') with
         | Some k1', Some k2' -> Some ((k1', k2'), v)
         | _ -> None)
  |> Map.of_list

let keys (a : t) =
  let sk, dk = Map.bindings a |> List.map fst |> List.split in
  List.sort_uniq Key.compare (sk @ dk)

let refine_memrefs (a : t) (rs : RegIdSet.t) : t =
  Map.to_list a
  |> List.filter_map (fun ((k1, k2), v) ->
         let k1' = Key.refine_memrefs k1 rs in
         let k2' = Key.refine_memrefs k2 rs in
         match (k1', k2') with
         | Some k1', Some k2' -> Some ((k1', k2'), v)
         | _ -> None)
  |> Map.of_list

let memory_keys (a : t) : AExprSet.t List.t =
  keys a
  |> List.filter_map (fun (k : Key.t) ->
         match k with KMemLoc v -> Some v | _ -> None)

let memory_base_regs (a : t) : RegIdSet.t =
  memory_keys a
  |> List.fold_left
       (fun (acc : RegIdSet.t) (k : AExprSet.t) ->
         RegIdSet.union (AExprSet.used_regs k) acc)
       RegIdSet.empty

let floyd_warshall graph =
  (* Assuming a function that returns all the keys (nodes) in the map *)
  let nodes = keys graph in

  let update_map intermediate graph =
    List.fold_left
      (fun acc_map i ->
        List.fold_left
          (fun acc_inner_map j ->
            let ik =
              Map.find_opt (i, intermediate) graph
              |> Option.value ~default:UpperD.Top
            in
            let kj =
              Map.find_opt (intermediate, j) graph
              |> Option.value ~default:UpperD.Top
            in
            let ij =
              Map.find_opt (i, j) graph |> Option.value ~default:UpperD.Top
            in
            let new_dist = UpperD.add ik kj in
            let updated_dist = UpperD.meet ij new_dist in
            Map.add (i, j) updated_dist acc_inner_map)
          acc_map nodes)
      graph nodes
  in
  List.fold_left (fun acc_graph k -> update_map k acc_graph) graph nodes

let join a b =
  let amregs = memory_base_regs a in
  let bmregs = memory_base_regs b in
  let inters =
    RegIdSet.inter amregs bmregs |> RegIdSet.elements |> List.sort_uniq compare
  in
  match inters with
  | [] -> Map.join (clear_memref a) (clear_memref b) |> floyd_warshall
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
        RegIdSet.union
          (AExprSet.used_regs amreg_rep)
          (AExprSet.used_regs bmreg_rep)
      in
      let final_inters =
        RegIdSet.filter
          (fun r -> AExprSet.has_same_diff amreg_rep bmreg_rep base r)
          candids
      in
      Map.join (refine_memrefs a final_inters) (refine_memrefs b final_inters)
      |> floyd_warshall

let le = Map.le
let top = Map.top
let meet a b = Map.meet a b |> floyd_warshall

let ole (m1 : t) (m2 : t) : bool =
  Map.fold
    (fun k v2 acc ->
      match Map.find_opt k m1 with
      | Some v1 -> acc && UpperD.le v1 v2
      | None -> acc && UpperD.le UpperD.top v2)
    m2 true

let pp fmt (a : t) =
  Map.iter
    (fun (k1, k2) v ->
      match v with
      | UpperD.Bounded _ ->
          Format.fprintf fmt "(%a - %a) <= %a; " Key.pp k1 Key.pp k2 UpperD.pp v
      | _ -> ())
    a

let find_all_equiv (a : t) (r : RegId.t) : AExprSet.t =
  let keys = keys a in
  List.fold_left
    (fun acc (k : Key.t) ->
      match k with
      | KReg r' -> (
          match (Map.find_opt (KReg r, k) a, Map.find_opt (k, KReg r) a) with
          | Some (UpperD.Bounded i), Some (UpperD.Bounded j) ->
              if i = Int64.neg j then AExprSet.add { base = r'; offset = i } acc
              else acc
          | _ -> acc)
      | _ -> acc)
    AExprSet.empty keys
  |> AExprSet.add { base = r; offset = 0L }

let rewrite_alias (a : t) : t =
  let regs = memory_base_regs a in
  if RegIdSet.is_empty regs then a
  else
    let reg = RegIdSet.choose regs in
    let aset = find_all_equiv a reg in
    Map.to_list a
    |> List.map (fun ((k1, k2), v) ->
           let k1' = Key.shift_aset k1 aset in
           let k2' = Key.shift_aset k2 aset in
           ((k1', k2'), v))
    |> Map.of_list

let add_single_eq (a : t) (newk : Key.t) (orig : Key.t) (offset : Int64.t) =
  let keys = keys a in
  let x =
    List.fold_left
      (fun acc k ->
        if Key.compare k orig = 0 then acc
        else
          Map.update (k, newk)
            (fun vo ->
              match (vo, Map.find_opt (k, orig) a) with
              | Some v, Some (UpperD.Bounded i) ->
                  Some (UpperD.meet v (UpperD.Bounded (Int64.sub i offset)))
              | Some v, _ -> Some v
              | _, Some (UpperD.Bounded i) ->
                  Some (UpperD.Bounded (Int64.sub i offset))
              | _ -> Some UpperD.Top)
            acc)
      a keys
  in
  let y =
    List.fold_left
      (fun acc k ->
        if Key.compare k orig = 0 then acc
        else
          Map.update (newk, k)
            (fun vo ->
              match (vo, Map.find_opt (orig, k) a) with
              | Some v, Some (UpperD.Bounded i) ->
                  Some (UpperD.meet v (UpperD.Bounded (Int64.add i offset)))
              | Some v, _ -> Some v
              | _, Some (UpperD.Bounded i) ->
                  Some (UpperD.Bounded (Int64.add i offset))
              | _ -> Some UpperD.Top)
            acc)
      x keys
  in
  y
  |> Map.add (orig, newk) (UpperD.Bounded (Int64.neg offset))
  |> Map.add (newk, orig) (UpperD.Bounded offset)

let update_single_reg (a : t) (orig : RegId.t) (offset : Int64.t) : t =
  let mem_updated =
    Map.to_list a
    |> List.map (fun ((k1, k2), v) ->
           let k1' = Key.update_single_reg k1 orig offset in
           let k2' = Key.update_single_reg k2 orig offset in
           match (k1', k2') with
           | KReg r1, KReg r2 ->
               if RegId.compare r1 orig = 0 && RegId.compare r2 orig = 0 then
                 ((k1', k2'), v)
               else if RegId.compare r1 orig = 0 then
                 ((k1', k2'), UpperD.map (fun x -> Int64.add x offset) v)
               else if RegId.compare r2 orig = 0 then
                 ((k1', k2'), UpperD.map (fun x -> Int64.sub x offset) v)
               else ((k1', k2'), v)
           | KReg r1, _ ->
               if RegId.compare r1 orig = 0 then
                 ((k1', k2'), UpperD.map (fun x -> Int64.add x offset) v)
               else ((k1', k2'), v)
           | _, KReg r2 ->
               if RegId.compare r2 orig = 0 then
                 ((k1', k2'), UpperD.map (fun x -> Int64.sub x offset) v)
               else ((k1', k2'), v)
           | _, _ -> ((k1', k2'), v))
    |> Map.of_list
  in
  mem_updated

let gen_single_lt (a : t) (r : RegId.t) (c : int64) : t =
  let keys = keys a in
  let x =
    List.fold_left
      (fun (acc : t) k ->
        acc
        |> Map.update (k, KZero) (fun vo ->
               match Map.find_opt (k, KReg r) a with
               | Some (UpperD.Bounded i) -> (
                   let nv = Int64.add i (Int64.pred c) in
                   match vo with
                   | Some (UpperD.Bounded j) ->
                       Some (UpperD.Bounded (Int64.min j nv))
                   | _ -> Some (UpperD.Bounded nv))
               | _ -> vo)
        |> Map.update (KReg r, k) (fun vo ->
               match Map.find_opt (KZero, k) a with
               | Some (UpperD.Bounded i) -> (
                   let nv = Int64.add i c in
                   match vo with
                   | Some (UpperD.Bounded j) ->
                       Some (UpperD.Bounded (Int64.min j nv))
                   | _ -> Some (UpperD.Bounded nv))
               | _ -> vo))
      a keys
  in
  x |> Map.add (KReg r, KZero) (UpperD.Bounded (Int64.pred c))

let gen_single_ge (a : t) (r : RegId.t) (c : int64) : t =
  let keys = keys a in
  let x =
    List.fold_left
      (fun (acc : t) k ->
        acc
        |> Map.update (KZero, k) (fun vo ->
               match Map.find_opt (KReg r, k) a with
               | Some (UpperD.Bounded i) -> (
                   let nv = Int64.add i (Int64.neg c) in
                   match vo with
                   | Some (UpperD.Bounded j) ->
                       Some (UpperD.Bounded (Int64.min j nv))
                   | _ -> Some (UpperD.Bounded nv))
               | _ -> vo)
        |> Map.update (k, KReg r) (fun vo ->
               match Map.find_opt (k, KZero) a with
               | Some (UpperD.Bounded i) -> (
                   let nv = Int64.add i c in
                   match vo with
                   | Some (UpperD.Bounded j) ->
                       Some (UpperD.Bounded (Int64.min j nv))
                   | _ -> Some (UpperD.Bounded nv))
               | _ -> vo))
      a keys
  in
  x |> Map.add (KZero, KReg r) (UpperD.Bounded (Int64.neg c))

let gen_single_eq (a : t) (r : RegId.t) (c : int64) : t =
  add_single_eq a (KReg r) KZero c

let request_interval (a : t) (r : RegId.t) =
  let minv =
    Map.find_opt (KZero, KReg r) a
    |> Option.map (fun v ->
           match v with
           | UpperD.Bounded i -> IntervalD.EInt (Int64.neg i)
           | _ -> ETop)
    |> Option.value ~default:IntervalD.ETop
  in
  let maxv =
    Map.find_opt (KReg r, KZero) a
    |> Option.map (fun v ->
           match v with UpperD.Bounded i -> IntervalD.EInt i | _ -> ETop)
    |> Option.value ~default:IntervalD.ETop
  in
  (minv, maxv)

let process_load (_ : Addr.t -> Char.t) (a : t) (outv : RegId.t_width)
    (addrSet : AExprSet.t) =
  [%log debug "process_load: %a <- %a" RegId.pp outv.id AExprSet.pp addrSet];
  let retv =
    let a = clear_mr a outv.id in
    add_single_eq a (KReg outv.id) (KMemLoc addrSet) 0L
  in
  retv |> rewrite_alias

let process_assignment (a : t) (asn : Assignable.t) (outv : RegId.t_width) =
  (let na = clear_mr a outv.id in
   match asn with
   | Avar (Register r) -> add_single_eq na (KReg outv.id) (KReg r.id) 0L
   | Avar _ -> na
   | Abop (Bint_add, op1v, op2v) -> (
       match (op1v, op2v) with
       | Register r, Const { value = c; _ } -> (
           if RegId.compare r.id outv.id = 0 then update_single_reg a r.id c
           else
             match
               ( Map.find_opt (KReg outv.id, KReg r.id) a,
                 Map.find_opt (KReg r.id, KReg outv.id) a )
             with
             | Some (UpperD.Bounded i1), Some (UpperD.Bounded i2) ->
                 if c = i1 && c = Int64.neg i2 then a
                 else add_single_eq na (KReg outv.id) (KReg r.id) c
             | _ -> add_single_eq na (KReg outv.id) (KReg r.id) c)
       | _ -> na)
   | Abop (Bint_sub, op1v, op2v) -> (
       match (op1v, op2v) with
       | Register r, Const { value = c; _ } ->
           if RegId.compare r.id outv.id = 0 then
             update_single_reg a r.id (Int64.neg c)
           else add_single_eq na (KReg outv.id) (KReg r.id) (Int64.neg c)
       | _ -> na)
   | Abop (_, _, _) -> na
   | Auop (Uint_zext, Register r) ->
       add_single_eq na (KReg outv.id) (KReg r.id) 0L
   | Auop (Uint_sext, Register r) ->
       add_single_eq na (KReg outv.id) (KReg r.id) 0L
   | Auop (_, _) -> na)
  |> rewrite_alias

let process_store (a : t) (vn : VarNode.t) (addrSet : AExprSet.t) : t =
  (match vn with
  | Register r -> add_single_eq a (KMemLoc addrSet) (KReg r.id) 0L
  | _ -> a)
  |> rewrite_alias
