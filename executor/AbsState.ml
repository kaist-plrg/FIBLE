open DS;;

module RegTopMapD = struct
  type t = AbsVal.t Int64Map.t
  let join a b = Int64Map.merge (fun _ a b -> match a,b with
    | Some a, Some b -> Some (AbsVal.join a b)
    | _, _ -> None
  ) a b
  let top = Int64Map.empty

  let ole (m1: t) (m2: t): bool =
    Int64Map.fold (fun k v2 acc -> match Int64Map.find_opt k m1 with
     | Some v1 -> acc && (AbsVal.ole v1 v2)
     | None -> acc && (AbsVal.ole AbsVal.top v2)
    ) m2 true
  let ole _ _ = true
end

type t = {
  value_nonrel: NonRelStateD.t;
  value_octagon: OctagonD.t;
  value_boolpower: BoolPowerD.t;
}

let join a b = {
  value_nonrel = NonRelStateD.join a.value_nonrel b.value_nonrel;
  value_octagon = OctagonD.join a.value_octagon b.value_octagon;
  value_boolpower = BoolPowerD.join a.value_boolpower b.value_boolpower;
}
let top = {
  value_nonrel = NonRelStateD.top;
  value_octagon = OctagonD.top;
  value_boolpower = BoolPowerD.top;
}

let pp fmt a = 
  Format.fprintf fmt "NonRelState: %a\nOctagon: %a\nBoolPower: %a\n" NonRelStateD.pp a.value_nonrel OctagonD.pp a.value_octagon BoolPowerD.pp a.value_boolpower

let ole a b = (NonRelStateD.ole a.value_nonrel b.value_nonrel) && (OctagonD.ole a.value_octagon b.value_octagon) && (BoolPowerD.ole a.value_boolpower b.value_boolpower)


let post_contour_single (p: PCode.prog) (ls: PCode.loc) (lf: PCode.loc) (a: t)(i: PCode.inst): t = 
  if (lf = (0x10073cL, 0)) then Format.printf "astate: %a\n" pp a else ();
  match i with
  | PCode.Iassignment (asn, outputv) -> {
    value_nonrel = NonRelStateD.process_assignment a.value_nonrel a.value_octagon asn outputv;
    value_octagon = OctagonD.process_assignment a.value_octagon asn outputv;
    value_boolpower = BoolPowerD.process_assignment a.value_boolpower a.value_octagon asn outputv;
  }
  | PCode.Iload (spacev, pointerv, outputv) -> {
    value_nonrel = NonRelStateD.process_load a.value_nonrel a.value_octagon pointerv outputv;
    value_octagon = OctagonD.process_load a.value_octagon pointerv outputv;
    value_boolpower = a.value_boolpower;
  }
  | PCode.Istore (spacev, pointerv, valuev) -> {
      value_nonrel = NonRelStateD.clear_memref a.value_nonrel;
      value_octagon = OctagonD.clear_memref a.value_octagon;
      value_boolpower = BoolPowerD.clear_memref a.value_boolpower;
    }
  | PCode.Icbranch (condv, truelocv) -> (match truelocv with
  |  {
    varNode_node = Ram trueloc ;
    varNode_width = _ } ->
      (match condv.varNode_node with
            | Unique i -> (match (MemRefTopMap.find_opt (MemRef.UniqueR i) a.value_boolpower) with
              | Some b ->
                if ((compare (trueloc, 0) lf) = 0)
                  then {
                    value_nonrel = a.value_nonrel;
                    value_octagon = OctagonD.meet a.value_octagon (fst b);
                    value_boolpower = a.value_boolpower;
                  } else {
                    value_nonrel = a.value_nonrel;
                    value_octagon = OctagonD.meet a.value_octagon (snd b);
                    value_boolpower = a.value_boolpower;
                  }
                  | _ -> a
                )
                | _ -> a
              )
  | _ -> a)
  | PCode.Ijump (ja, locv) -> a
  | PCode.Ijump_ind (jia, locv) -> a
  | PCode.INop -> a
  | PCode.Iunimplemented -> a
