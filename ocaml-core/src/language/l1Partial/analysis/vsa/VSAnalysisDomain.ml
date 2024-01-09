open StdlibExt
open Basic
open Basic_collection
open Basic_domain
open Value_domain

type __ = {
  value_nonrel : NonRelStateD.t;
  value_octagon : OctagonD.t;
  value_boolpower : BoolPowerD.t;
}

module Lattice_noBot = struct
  include
    TripleD.MakeJoinSemiLatticeWithTop_Record (NonRelStateD) (OctagonD)
      (BoolPowerD)
      (struct
        type t = __

        let get_fst a = a.value_nonrel
        let get_snd a = a.value_octagon
        let get_trd a = a.value_boolpower

        let make a b c =
          { value_nonrel = a; value_octagon = b; value_boolpower = c }
      end)

  let equal (a : t) (b : t) =
    NonRelStateD.le a.value_nonrel b.value_nonrel
    && NonRelStateD.le b.value_nonrel a.value_nonrel

  let pp fmt (a : t) =
    Format.fprintf fmt "NonRelState: %a\nOctagon: %a\nBoolPower: %a\n"
      NonRelStateD.pp a.value_nonrel OctagonD.pp a.value_octagon BoolPowerD.pp
      a.value_boolpower

  let gen_aexpr_set (o : OctagonD.t) (v : VarNode.t) : AExprSet.t =
    match v with
    | Register r -> OctagonD.find_all_equiv o r.id
    | _ -> AExprSet.empty

  let clear_memref (a : t) : t =
    {
      value_nonrel = NonRelStateD.clear_memref a.value_nonrel;
      value_octagon = OctagonD.clear_memref a.value_octagon;
      value_boolpower = BoolPowerD.clear_memref a.value_boolpower;
    }

  let post_single (rom : Addr.t -> Char.t) (ls : Loc.t) (a : t) (i : Inst.t) : t
      =
    match i with
    | Iassignment { expr; output } ->
        {
          value_nonrel =
            NonRelStateD.process_assignment a.value_nonrel a.value_octagon expr
              output;
          value_octagon =
            OctagonD.process_assignment a.value_octagon expr output;
          value_boolpower =
            BoolPowerD.process_assignment a.value_boolpower a.value_octagon expr
              output;
        }
    | Iload { pointer; output; _ } ->
        let addrSet = gen_aexpr_set a.value_octagon pointer in
        let regSet = AExprSet.used_regs addrSet in
        let inter_regSet =
          RegIdSet.inter regSet (OctagonD.memory_base_regs a.value_octagon)
        in
        let inter_addrSet =
          AExprSet.filter
            (fun { base; offset } -> RegIdSet.mem base inter_regSet)
            addrSet
        in
        let a, addrSet =
          if AExprSet.is_empty inter_addrSet then (clear_memref a, addrSet)
          else (a, inter_addrSet)
        in

        {
          value_nonrel =
            NonRelStateD.process_load rom a.value_nonrel a.value_octagon output
              addrSet;
          value_octagon =
            OctagonD.process_load rom a.value_octagon output addrSet;
          value_boolpower =
            BoolPowerD.process_load rom a.value_boolpower a.value_octagon output
              addrSet;
        }
    | Istore { pointer; value; _ } ->
        let addrSet = gen_aexpr_set a.value_octagon pointer in
        let regSet = AExprSet.used_regs addrSet in
        let inter_regSet =
          RegIdSet.inter regSet (OctagonD.memory_base_regs a.value_octagon)
        in
        let inter_addrSet =
          AExprSet.filter
            (fun { base; offset } -> RegIdSet.mem base inter_regSet)
            addrSet
        in
        let a, addrSet =
          if AExprSet.is_empty inter_addrSet then (clear_memref a, addrSet)
          else (a, inter_addrSet)
        in
        {
          value_nonrel =
            NonRelStateD.process_store a.value_nonrel a.value_octagon value
              addrSet;
          value_octagon = OctagonD.process_store a.value_octagon value addrSet;
          value_boolpower =
            BoolPowerD.process_store a.value_boolpower a.value_octagon value
              addrSet;
        }
    | INop -> a

  let filter_branch (a : t) (condv : VarNode.t) (trueloc : Loc.t)
      (targetloc : Loc.t) : t =
    match condv with
    | Register r -> (
        match BoolPowerD.find_opt (KReg r.id) a.value_boolpower with
        | Some b ->
            if compare trueloc targetloc = 0 then
              {
                value_nonrel = a.value_nonrel;
                value_octagon = fst b;
                value_boolpower = a.value_boolpower;
              }
            else
              {
                value_nonrel = a.value_nonrel;
                value_octagon = snd b;
                value_boolpower = a.value_boolpower;
              }
        | _ -> a)
    | _ -> a

  let try_concretize_vn (a : t) (vn : VarNode.t) : Int64Set.t option =
    match vn with
    | Register u ->
        OctagonD.find_all_equiv a.value_octagon u.id
        |> AExprSet.to_list
        |> List.fold_left
             (fun acc (ae : AExpr.t) ->
               match acc with
               | Some _ -> acc
               | None -> (
                   match
                     NonRelStateD.find_opt (KReg ae.base) a.value_nonrel
                   with
                   | Some a ->
                       AbsNumeric.try_concretize a
                       |> Option.map (fun x ->
                              Int64Set.map (fun x -> Int64.add x ae.offset) x)
                   | None -> None))
             None
    | _ -> None
end

type t = LV of (Lattice_noBot.t * (Addr.t -> Char.t)) | Bottom
type edge = ICFG.G.E.t

let join a b =
  match (a, b) with
  | LV a, LV b -> LV (Lattice_noBot.join (fst a) (fst b), snd a)
  | Bottom, _ -> b
  | _, Bottom -> a

let widening a b =
  match (a, b) with
  | LV a, LV b -> LV (Lattice_noBot.top, snd a)
  | Bottom, _ -> b
  | _, Bottom -> a

let pp fmt (a : t) =
  match a with
  | LV a -> Lattice_noBot.pp fmt (fst a)
  | Bottom -> Format.fprintf fmt "Bottom"

let bot = Bottom
let init (rom : Addr.t -> Char.t) = LV (Lattice_noBot.top, rom)

let equal a b =
  match (a, b) with
  | LV a, LV b -> Lattice_noBot.equal (fst a) (fst b)
  | Bottom, Bottom -> true
  | _ -> false

let analyze_noBot (e : edge) (a : Lattice_noBot.t) (rom : Addr.t -> Char.t) :
    Lattice_noBot.t =
  match ICFG.G.E.label e with
  | Inner ->
      List.fold_left
        (fun a (i : Inst.t_full) -> Lattice_noBot.post_single rom i.loc a i.ins)
        a (ICFG.G.E.src e).block.body
  | Flow -> (
      let srcBlock = ICFG.G.E.src e in
      match srcBlock.block.jmp.jmp with
      | Jcbranch { condition; target_true; target_false } ->
          Lattice_noBot.filter_branch a condition target_true
            (ICFG.G.E.dst e).block.loc
      | _ -> a)

let analyze (e : edge) (a : t) : t =
  match a with
  | LV a -> LV (analyze_noBot e (fst a) (snd a), snd a)
  | Bottom -> Bottom
