open StdlibExt
open Basic
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

  let post_single (p : Prog.t) (ls : Loc.t) (a : t) (i : Inst.t) : t =
    match i with
    | Iassignment (asn, outputv) ->
        {
          value_nonrel =
            NonRelStateD.process_assignment a.value_nonrel a.value_octagon asn
              outputv;
          value_octagon =
            OctagonD.process_assignment a.value_octagon asn outputv;
          value_boolpower =
            BoolPowerD.process_assignment a.value_boolpower a.value_octagon asn
              outputv;
        }
    | Iload (_, pointerv, outputv) ->
        {
          value_nonrel =
            NonRelStateD.process_load p a.value_nonrel a.value_octagon pointerv
              outputv;
          value_octagon =
            OctagonD.process_load p a.value_octagon pointerv outputv;
          value_boolpower =
            BoolPowerD.process_load p a.value_boolpower a.value_octagon pointerv
              outputv;
        }
    | Istore (_, pointerv, storev) ->
        let addrSet = gen_aexpr_set a.value_octagon pointerv in
        {
          value_nonrel = NonRelStateD.clear_memref a.value_nonrel;
          value_octagon = OctagonD.clear_memref a.value_octagon;
          value_boolpower = BoolPowerD.clear_memref a.value_boolpower;
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
                value_octagon = OctagonD.meet a.value_octagon (fst b);
                value_boolpower = a.value_boolpower;
              }
            else
              {
                value_nonrel = a.value_nonrel;
                value_octagon = OctagonD.meet a.value_octagon (snd b);
                value_boolpower = a.value_boolpower;
              }
        | _ -> a)
    | _ -> a

  let try_concretize_vn (a : t) (vn : VarNode.t) (limit : int) :
      Int64Set.t option =
    match vn with
    | Register u -> (
        match NonRelStateD.find_opt (KReg u.id) a.value_nonrel with
        | Some a -> AbsNumeric.try_concretize a limit
        | None -> None)
    | _ -> None
end

type t = LV of (Lattice_noBot.t * Prog.t) | Bottom
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
let init (p : Prog.t) = LV (Lattice_noBot.top, p)

let equal a b =
  match (a, b) with
  | LV a, LV b -> Lattice_noBot.equal (fst a) (fst b)
  | Bottom, Bottom -> true
  | _ -> false

let analyze_noBot (e : edge) (a : Lattice_noBot.t) (p : Prog.t) :
    Lattice_noBot.t =
  match ICFG.G.E.label e with
  | Inner ->
      List.fold_left
        (fun a (i : Inst.t_full) -> Lattice_noBot.post_single p i.loc a i.ins)
        a (ICFG.G.E.src e).block.body
  | Flow -> (
      let srcBlock = ICFG.G.E.src e in
      match srcBlock.block.jmp.jmp with
      | Jcbranch (cv, trueLoc, falseLoc) ->
          Lattice_noBot.filter_branch a cv trueLoc (ICFG.G.E.dst e).block.loc
      | _ -> a)

let analyze (e : edge) (a : t) : t =
  match a with
  | LV a -> LV (analyze_noBot e (fst a) (snd a), snd a)
  | Bottom -> Bottom
