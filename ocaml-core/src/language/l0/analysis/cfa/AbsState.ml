open StdlibExt
open Basic
open Basic_domain
open Value_domain

type __ = {
  value_nonrel : NonRelStateD.t;
  value_octagon : OctagonD.t;
  value_boolpower : BoolPowerD.t;
}

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

let widen _ _ = top

let pp fmt (a : t) =
  Format.fprintf fmt "NonRelState: %a\nOctagon: %a\nBoolPower: %a\n"
    NonRelStateD.pp a.value_nonrel OctagonD.pp a.value_octagon BoolPowerD.pp
    a.value_boolpower

let post_single (p : Prog.t) (ls : Loc.t) (a : t) (i : Inst.t) : t =
  match i with
  | Iassignment { expr; output } ->
      {
        value_nonrel =
          NonRelStateD.process_assignment a.value_nonrel a.value_octagon expr
            output;
        value_octagon = OctagonD.process_assignment a.value_octagon expr output;
        value_boolpower =
          BoolPowerD.process_assignment a.value_boolpower a.value_octagon expr
            output;
      }
  | Iload { pointer; output } ->
      {
        value_nonrel =
          NonRelStateD.process_load p a.value_nonrel a.value_octagon pointer
            output;
        value_octagon = OctagonD.process_load p a.value_octagon pointer output;
        value_boolpower = a.value_boolpower;
      }
  | Istore _ ->
      {
        value_nonrel = NonRelStateD.clear_memref a.value_nonrel;
        value_octagon = OctagonD.clear_memref a.value_octagon;
        value_boolpower = BoolPowerD.clear_memref a.value_boolpower;
      }
  | Icbranch _ -> a
  | Ijump _ -> a
  | Ijump_ind _ -> a
  | INop -> a
  | Iunimplemented -> a

let filter_single (_ : Prog.t) (_ : Loc.t) (lf : Loc.t) (a : t) (i : Inst.t) : t
    =
  match i with
  | Icbranch { condition; target } -> (
      match condition with
      | Register { id = RegId.Unique _ as i; _ } -> (
          match BoolPowerD.find_opt (MemRef.R i) a.value_boolpower with
          | Some b ->
              if compare target lf = 0 then
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
      | _ -> a)
  | _ -> a

let try_concretize_vn (a : t) (vn : VarNode.t) : Int64Set.t option =
  match vn with
  | Register u -> (
      match NonRelStateD.find_opt (MemRef.R u.id) a.value_nonrel with
      | Some a -> AbsNumeric.try_concretize a
      | None -> None)
  | _ -> None
