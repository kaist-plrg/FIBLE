open StdlibExt
open Common

type t = Top | InitHoleMap of ASymb.t AddrMap.t | Bottom

let join (c1 : t) (c2 : t) : t =
  match (c1, c2) with
  | Top, _ | _, Top -> Top
  | InitHoleMap m1, InitHoleMap m2 ->
      InitHoleMap
        (AddrMap.merge
           (fun key v1 v2 ->
             match (v1, v2) with
             | Some v1, Some v2 -> Some (ASymb.join v1 v2)
             | Some v, None ->
                 Some (ASymb.join v (ASymb.AStackOffset (key, Int64Set.empty)))
             | None, Some v ->
                 Some (ASymb.join (ASymb.AStackOffset (key, Int64Set.empty)) v)
             | None, None -> None)
           m1 m2)
  | Bottom, _ | _, Bottom -> Bottom

let le (c1 : t) (c2 : t) : bool =
  match (c1, c2) with
  | _, Top -> true
  | Top, _ -> false
  | InitHoleMap m1, InitHoleMap m2 ->
      AddrMap.for_all
        (fun r s1 ->
          let s2 =
            AddrMap.find_opt r m2
            |> Option.value ~default:(ASymb.AStackOffset (r, Int64Set.empty))
          in
          ASymb.le s1 s2)
        m1
      && AddrMap.for_all
           (fun r s2 ->
             let s1 =
               AddrMap.find_opt r m1
               |> Option.value ~default:(ASymb.AStackOffset (r, Int64Set.empty))
             in
             ASymb.le s1 s2)
           m2
  | Bottom, _ -> true
  | _, Bottom -> false

let top = Top

let process_sload (c : t) (offset : Int64.t) : ASymb.t =
  match c with
  | Top -> ASymb.Top
  | InitHoleMap m ->
      AddrMap.find_opt offset m
      |> Option.value ~default:(ASymb.AStackOffset (offset, Int64Set.empty))
  | Bottom -> ASymb.Bottom

let process_sstore (c : t) (offset : Int64.t) (v : ASymb.t) : t =
  match c with
  | Top -> Top
  | InitHoleMap m -> InitHoleMap (AddrMap.add offset v m)
  | Bottom -> Bottom
