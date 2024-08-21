open Common

type t = KReg of RegId.t_full | KMemLoc of AExprSet.t | KZero

let pp fmt (a : t) =
  match a with
  | KReg r -> Format.fprintf fmt "%a" RegId.pp_full r
  | KMemLoc s -> Format.fprintf fmt "[%a]" AExprSet.pp s
  | KZero -> Format.fprintf fmt "0"

let refine_memrefs (a : t) (memrefs : RegIdFullSet.t) : t Option.t =
  match a with
  | KMemLoc s ->
      let s' = AExprSet.filter (fun e -> RegIdFullSet.mem e.base memrefs) s in
      if AExprSet.is_empty s' then None else Some (KMemLoc s')
  | _ -> Some a

let clear_mr (a : t) (r : RegId.t_full) : t Option.t =
  match a with
  | KReg r' -> if RegId.compare r.id r'.id = 0 then None else Some a
  | KMemLoc s ->
      let s' = AExprSet.filter (fun e -> RegId.compare e.base.id r.id <> 0) s in
      if AExprSet.is_empty s' then None else Some (KMemLoc s')
  | _ -> Some a

let update_single_reg (a : t) (r : RegId.t_full) (offset : Z.t) : t =
  match a with
  | KMemLoc s ->
      KMemLoc
        (AExprSet.map
           (fun e ->
             if RegId.compare e.base.id r.id = 0 then
               { e with offset = Z.sub e.offset offset }
             else e)
           s)
  | _ -> a

let shift_aset (a : t) (aset : AExprSet.t) : t =
  match a with
  | KMemLoc s ->
      let diff =
        AExprSet.choose s |> fun e ->
        Z.sub e.offset
          ( AExprSet.find_filter_opt
              (fun x -> RegId.compare x.base.id e.base.id = 0)
              aset
          |> Option.map (fun v _ -> v)
          |> Option.value ~default:(fun () ->
                 [%log
                   fatal
                     "shift_aset: no matching base register %a found from %a"
                     RegId.pp_full e.base AExprSet.pp aset])
          |> fun f -> f () )
            .offset
      in
      KMemLoc
        (AExprSet.map (fun e -> { e with offset = Z.add e.offset diff }) aset)
  | _ -> a

let compare (a : t) (b : t) : Int.t =
  match (a, b) with
  | KReg r1, KReg r2 -> RegId.compare_full r1 r2
  | KMemLoc s1, KMemLoc s2 -> AExprSet.compare s1 s2
  | KZero, KZero -> 0
  | KReg _, _ -> -1
  | _, KReg _ -> 1
  | KMemLoc _, KZero -> -1
  | KZero, KMemLoc _ -> 1
