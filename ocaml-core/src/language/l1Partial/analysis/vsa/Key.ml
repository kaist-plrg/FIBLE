open Basic
open Basic_collection

type t = KReg of RegId.t | KMemLoc of AExprSet.t | KZero

let pp fmt (a : t) =
  match a with
  | KReg r -> Format.fprintf fmt "%a" RegId.pp r
  | KMemLoc s -> Format.fprintf fmt "%a" AExprSet.pp s
  | KZero -> Format.fprintf fmt "0"

let refine_memrefs (a : t) (memrefs : RegIdSet.t) : t Option.t =
  match a with
  | KMemLoc s ->
      let s' = AExprSet.filter (fun e -> RegIdSet.mem e.base memrefs) s in
      if AExprSet.is_empty s' then None else Some (KMemLoc s')
  | _ -> Some a

let clear_mr (a : t) (r : RegId.t) : t Option.t =
  match a with
  | KReg r' -> if RegId.compare r r' = 0 then None else Some a
  | KMemLoc s ->
      let s' = AExprSet.filter (fun e -> RegId.compare e.base r <> 0) s in
      if AExprSet.is_empty s' then None else Some (KMemLoc s')
  | _ -> Some a

let update_single_reg (a : t) (r : RegId.t) (offset : Int64.t) : t =
  match a with
  | KMemLoc s ->
      KMemLoc
        (AExprSet.map
           (fun e ->
             if RegId.compare e.base r = 0 then
               { e with offset = Int64.sub e.offset offset }
             else e)
           s)
  | _ -> a

let compare = compare
