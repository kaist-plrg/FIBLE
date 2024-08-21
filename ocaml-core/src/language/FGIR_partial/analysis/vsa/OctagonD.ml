open Common
open Basic_domain
open Value_domain
open Syn

type mband_t = { bits : Int.t; v : Z.t }

let get_mband_value (a : mband_t) : Z.t =
  Z.sub a.v (Z.shift_left Z.one (a.bits - 1))

module UpperD = struct
  type t = Bounded of mband_t | Top | Bot

  let compare = compare
  let top = Top
  let map f a = match a with Bounded i -> Bounded (f i) | _ -> a
  let bind f a = match a with Bounded i -> f i | _ -> a

  let mk_bounded (v : Z.t) (bits : Int.t) : t =
    let nv = Z.add v (Z.shift_left Z.one (bits - 1)) in
    if Z.zero <= nv && nv < Z.shift_left Z.one bits then
      Bounded { bits; v = nv }
    else Top

  let add a b =
    match (a, b) with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Bounded a, Bounded b ->
        if a.bits <> b.bits then Top
        else
          let v = Z.sub (Z.add a.v b.v) (Z.shift_left Z.one (a.bits - 1)) in
          if Z.zero <= v && v < Z.shift_left Z.one a.bits then
            Bounded { bits = a.bits; v }
          else Top
    | _, _ -> Top

  let join a b =
    match (a, b) with
    | Bot, _ -> b
    | _, Bot -> a
    | Bounded a, Bounded b ->
        if a.bits <> b.bits then Top
        else if a.v = b.v then Bounded a
        else Bounded { bits = a.bits; v = Z.max a.v b.v }
    | _, _ -> Top

  let meet a b =
    match (a, b) with
    | Top, _ -> b
    | _, Top -> a
    | Bounded a, Bounded b ->
        if a.bits <> b.bits then Bot
        else if a.v = b.v then Bounded a
        else Bounded { bits = a.bits; v = Z.min a.v b.v }
    | _, _ -> Bot

  let le a b =
    match (a, b) with
    | Bot, _ -> true
    | _, Bot -> false
    | Bounded a, Bounded b -> a.bits = b.bits && a.v <= b.v
    | Top, Bounded _ -> false
    | _, _ -> true

  let pp fmt a =
    match a with
    | Bounded i ->
        Format.fprintf fmt "%a" Z.pp_print
          (Z.sub i.v (Z.shift_left Z.one (i.bits - 1)))
    | Top -> Format.fprintf fmt "Top"
    | Bot -> Format.fprintf fmt "Bot"
end

let mband_add (a : mband_t) (v : Z.t) : UpperD.t =
  let nv = Z.add a.v v in
  if Z.zero <= nv && nv < Z.shift_left Z.one a.bits then
    Bounded { bits = a.bits; v = nv }
  else Top

let mband_sub (a : mband_t) (v : Z.t) : UpperD.t =
  let nv = Z.sub a.v v in
  if Z.zero <= nv && nv < Z.shift_left Z.one a.bits then
    Bounded { bits = a.bits; v = nv }
  else Top

module Map = struct
  include TopMapD.MakeLatticeWithTop (KeyPair) (UpperD)

  let find_filter_opt (f : KeyPair.t -> bool) (a : t) :
      (KeyPair.t * UpperD.t) Option.t =
    fold (fun k v acc -> if f k then Some (k, v) else acc) a None

  let find_loc_opt_1 (a : t) (r1 : AExpr.t) (r2 : RegId.t_full) :
      UpperD.t option =
    find_filter_opt
      (fun (k1, k2) ->
        match (k1, k2) with
        | KMemLoc v, KReg r -> AExprSet.mem r1 v && r = r2
        | _ -> false)
      a
    |> Option.map snd

  let find_loc_opt_2 (a : t) (r1 : RegId.t_full) (r2 : AExpr.t) :
      UpperD.t option =
    find_filter_opt
      (fun (k1, k2) ->
        match (k1, k2) with
        | KReg r, KMemLoc v -> r = r1 && AExprSet.mem r2 v
        | _ -> false)
      a
    |> Option.map snd
end

type t = Map.t

let pp fmt (a : t) =
  Map.iter
    (fun (k1, k2) v ->
      match v with
      | UpperD.Bounded _ ->
          Format.fprintf fmt "(%a - %a) <= %a; " Key.pp k1 Key.pp k2 UpperD.pp v
      | _ -> ())
    a

let clear_memref a =
  Map.filter
    (fun k _ ->
      match k with KMemLoc _, _ -> false | _, KMemLoc _ -> false | _ -> true)
    a

let clear_tempreg a =
  Map.filter
    (fun k _ ->
      match k with
      | KReg { id = Unique _; _ }, _ -> false
      | _, KReg { id = Unique _; _ } -> false
      | _ -> true)
    a

let clear_mr a (r : RegId.t_full) =
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

let refine_memrefs (a : t) (rs : RegIdFullSet.t) : t =
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

let memory_base_regs (a : t) : RegIdFullSet.t =
  memory_keys a
  |> List.fold_left
       (fun (acc : RegIdFullSet.t) (k : AExprSet.t) ->
         RegIdFullSet.union (AExprSet.used_regs k) acc)
       RegIdFullSet.empty

let floyd_warshall graph =
  (* Assuming a function that returns all the keys (nodes) in the map *)
  let nodes = keys graph in

  let update_map intermediate graph =
    List.fold_left
      (fun acc_map i ->
        if Key.compare i intermediate = 0 then acc_map
        else
          List.fold_left
            (fun acc_inner_map j ->
              if Key.compare j intermediate = 0 then acc_inner_map
              else if Key.compare i j = 0 then acc_inner_map
              else
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

let check_neg_cycle a =
  let a = a |> floyd_warshall in
  let nodes = keys a in
  List.exists
    (fun k ->
      let v = Map.find_opt (k, k) a in
      match v with
      | Some (UpperD.Bounded i) ->
          [%log debug "check_neg_cycle: %a" UpperD.pp (UpperD.Bounded i)];
          Z.compare (get_mband_value i) Z.zero < 0
      | _ -> false)
    nodes

let join a b =
  let amregs = memory_base_regs a in
  let bmregs = memory_base_regs b in
  let inters =
    RegIdFullSet.inter amregs bmregs
    |> RegIdFullSet.elements |> List.sort_uniq compare
  in
  match inters with
  | [] ->
      let res = Map.join (clear_memref a) (clear_memref b) |> floyd_warshall in
      res
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
      let res =
        Map.join (refine_memrefs a final_inters) (refine_memrefs b final_inters)
      in
      let res = res |> floyd_warshall in
      res

let le = Map.le
let top = Map.top

let meet a b =
  let amregs = memory_base_regs a in
  let bmregs = memory_base_regs b in
  let inters =
    RegIdFullSet.inter amregs bmregs
    |> RegIdFullSet.elements |> List.sort_uniq compare
  in
  match inters with
  | [] -> Map.meet (clear_memref a) (clear_memref b) |> floyd_warshall
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
      Map.meet (refine_memrefs a final_inters) (refine_memrefs b final_inters)
      |> floyd_warshall

let ole (m1 : t) (m2 : t) : bool =
  Map.fold
    (fun k v2 acc ->
      match Map.find_opt k m1 with
      | Some v1 -> acc && UpperD.le v1 v2
      | None -> acc && UpperD.le UpperD.top v2)
    m2 true

let find_all_equiv (a : t) (r : RegId.t_full) : AExprSet.t =
  let keys = keys a in
  List.fold_left
    (fun acc (k : Key.t) ->
      match k with
      | KReg r' -> (
          match (Map.find_opt (KReg r, k) a, Map.find_opt (k, KReg r) a) with
          | Some (UpperD.Bounded i), Some (UpperD.Bounded j) ->
              if get_mband_value i = Z.neg (get_mband_value j) then
                AExprSet.add { base = r'; offset = get_mband_value i } acc
              else acc
          | _ -> acc)
      | _ -> acc)
    AExprSet.empty keys
  |> AExprSet.add { base = r; offset = Z.zero }

let rewrite_alias (a : t) : t =
  let regs = memory_base_regs a in
  if RegIdFullSet.is_empty regs then a
  else
    let reg = RegIdFullSet.choose regs in
    let aset = find_all_equiv a reg in
    Map.to_list a
    |> List.map (fun ((k1, k2), v) ->
           let k1' = Key.shift_aset k1 aset in
           let k2' = Key.shift_aset k2 aset in
           ((k1', k2'), v))
    |> Map.of_list

let add_single_eq (a : t) (newk : Key.t) (orig : Key.t) (offset : Z.t)
    (bits : Int.t) =
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
                  Some (UpperD.meet v (mband_sub i offset))
              | Some v, _ -> Some v
              | _, Some (UpperD.Bounded i) -> Some (mband_sub i offset)
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
                  Some (UpperD.meet v (mband_add i offset))
              | Some v, _ -> Some v
              | _, Some (UpperD.Bounded i) -> Some (mband_add i offset)
              | _ -> Some UpperD.Top)
            acc)
      x keys
  in
  y
  |> Map.add (orig, newk) (UpperD.mk_bounded (Z.neg offset) bits)
  |> Map.add (newk, orig) (UpperD.mk_bounded offset bits)

let update_single_reg (a : t) (orig : RegId.t_full) (offset : Z.t) : t =
  let mem_updated =
    Map.to_list a
    |> List.map (fun ((k1, k2), v) ->
           let k1' = Key.update_single_reg k1 orig offset in
           let k2' = Key.update_single_reg k2 orig offset in
           match (k1', k2') with
           | KReg r1, KReg r2 ->
               if
                 RegId.compare_full r1 orig = 0
                 && RegId.compare_full r2 orig = 0
               then ((k1', k2'), v)
               else if RegId.compare_full r1 orig = 0 then
                 ((k1', k2'), UpperD.bind (fun x -> mband_add x offset) v)
               else if RegId.compare_full r2 orig = 0 then
                 ((k1', k2'), UpperD.bind (fun x -> mband_sub x offset) v)
               else ((k1', k2'), v)
           | KReg r1, _ ->
               if RegId.compare_full r1 orig = 0 then
                 ((k1', k2'), UpperD.bind (fun x -> mband_add x offset) v)
               else ((k1', k2'), v)
           | _, KReg r2 ->
               if RegId.compare_full r2 orig = 0 then
                 ((k1', k2'), UpperD.bind (fun x -> mband_sub x offset) v)
               else ((k1', k2'), v)
           | _, _ -> ((k1', k2'), v))
    |> Map.of_list
  in
  mem_updated

let gen_single_lt (a : t) (r : RegId.t_full) (c : Z.t) (bits : Int.t) : t =
  let keys = keys a in
  let x =
    List.fold_left
      (fun (acc : t) k ->
        acc
        |> Map.update (k, KZero) (fun vo ->
               match Map.find_opt (k, KReg r) a with
               | Some (UpperD.Bounded i) -> (
                   let nv = mband_add i (Z.pred c) in
                   match vo with
                   | Some (UpperD.Bounded j) ->
                       Some (UpperD.meet (UpperD.Bounded j) nv)
                   | _ -> Some nv)
               | _ -> vo)
        |> Map.update (KReg r, k) (fun vo ->
               match Map.find_opt (KZero, k) a with
               | Some (UpperD.Bounded i) -> (
                   let nv = mband_add i c in
                   match vo with
                   | Some (UpperD.Bounded j) ->
                       Some (UpperD.meet (UpperD.Bounded j) nv)
                   | _ -> Some nv)
               | _ -> vo))
      a keys
  in
  x |> Map.add (KReg r, KZero) (UpperD.mk_bounded (Z.pred c) bits)

let gen_single_ge (a : t) (r : RegId.t_full) (c : Z.t) (bits : Int.t) : t =
  let keys = keys a in
  let x =
    List.fold_left
      (fun (acc : t) k ->
        acc
        |> Map.update (KZero, k) (fun vo ->
               match Map.find_opt (KReg r, k) a with
               | Some (UpperD.Bounded i) -> (
                   let nv = mband_add i (Z.neg c) in
                   match vo with
                   | Some (UpperD.Bounded j) ->
                       Some (UpperD.meet (UpperD.Bounded j) nv)
                   | _ -> Some nv)
               | _ -> vo)
        |> Map.update (k, KReg r) (fun vo ->
               match Map.find_opt (k, KZero) a with
               | Some (UpperD.Bounded i) -> (
                   let nv = mband_add i c in
                   match vo with
                   | Some (UpperD.Bounded j) ->
                       Some (UpperD.meet (UpperD.Bounded j) nv)
                   | _ -> Some nv)
               | _ -> vo))
      a keys
  in
  x |> Map.add (KZero, KReg r) (UpperD.mk_bounded (Z.neg c) bits)

let gen_single_eq (a : t) (r : RegId.t_full) (c : Z.t) (bits : Int.t) : t =
  add_single_eq a (KReg r) KZero c bits

let request_interval (a : t) (r : RegId.t_full) =
  let minv =
    Map.find_opt (KZero, KReg r) a
    |> Option.map (fun v ->
           match v with
           | UpperD.Bounded i -> IntervalD.LE (Z.neg (get_mband_value i))
           | _ -> MInf)
    |> Option.value ~default:IntervalD.MInf
  in
  let maxv =
    Map.find_opt (KReg r, KZero) a
    |> Option.map (fun v ->
           match v with
           | UpperD.Bounded i -> IntervalD.RE (get_mband_value i)
           | _ -> IntervalD.Inf)
    |> Option.value ~default:IntervalD.Inf
  in
  [%log
    debug "request_interval: %a -> (%a, %a)" RegId.pp_full r IntervalD.pp_left
      minv IntervalD.pp_right maxv];
  IntervalD.make minv maxv

let process_load (_ : DMem.t) (a : t) (outv : RegId.t_full)
    (addrSet : AExprSet.t) =
  [%log debug "process_load: %a <- %a" RegId.pp outv.id AExprSet.pp addrSet];
  let retv =
    let a = clear_mr a outv in
    add_single_eq a (KReg outv) (KMemLoc addrSet) Z.zero
      (Int32.to_int outv.width * 8)
  in
  retv |> rewrite_alias

let process_assignment (a : t) (asn : Assignable.t) (outv : RegId.t_full) =
  (let na = clear_mr a outv in
   match asn with
   | Avar (Register r) ->
       if RegId.compare r.id outv.id = 0 then a
       else
         add_single_eq na (KReg outv) (KReg r) Z.zero (Int32.to_int r.width * 8)
   | Avar _ -> na
   | Abop (Bint_add, op1v, op2v) -> (
       match (op1v, op2v) with
       | Register r, Const { value = c; _ } -> (
           let c = Z.of_int64 c in
           if RegId.compare r.id outv.id = 0 then update_single_reg a r c
           else
             match
               ( Map.find_opt (KReg outv, KReg r) a,
                 Map.find_opt (KReg r, KReg outv) a )
             with
             | Some (UpperD.Bounded i1), Some (UpperD.Bounded i2) ->
                 if c = get_mband_value i1 && c = Z.neg (get_mband_value i2)
                 then a
                 else
                   add_single_eq na (KReg outv) (KReg r) c
                     (Int32.to_int r.width * 8)
             | _ ->
                 add_single_eq na (KReg outv) (KReg r) c
                   (Int32.to_int r.width * 8))
       | _ -> na)
   | Abop (Bint_sub, op1v, op2v) -> (
       match (op1v, op2v) with
       | Register r, Const { value = c; _ } ->
           let c = Z.of_int64 c in
           if RegId.compare r.id outv.id = 0 then
             update_single_reg a r (Z.neg c)
           else
             add_single_eq na (KReg outv) (KReg r) (Z.neg c)
               (Int32.to_int r.width * 8)
       | _ -> na)
   | Abop (_, _, _) -> na
   | Auop (Uint_zext, Register r) ->
       if RegId.compare r.id outv.id = 0 then a
       else
         add_single_eq na (KReg outv) (KReg r) Z.zero (Int32.to_int r.width * 8)
   | Auop (Uint_sext, Register r) ->
       if RegId.compare r.id outv.id = 0 then a
       else
         add_single_eq na (KReg outv) (KReg r) Z.zero (Int32.to_int r.width * 8)
   | Auop (_, _) -> na)
  |> rewrite_alias

let process_store (a : t) (vn : Common.NumericVarNode.t) (addrSet : AExprSet.t)
    : t =
  (match vn with
  | Register r ->
      add_single_eq a (KMemLoc addrSet) (KReg r) Z.zero
        (Int32.to_int r.width * 8)
  | _ -> a)
  |> rewrite_alias
