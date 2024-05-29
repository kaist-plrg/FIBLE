open StdlibExt
open Common

type t = TopHoleMap of AbsVal.t Byte8Map.t | Bottom

let join (c1 : t) (c2 : t) : t =
  match (c1, c2) with
  | TopHoleMap m1, TopHoleMap m2 ->
      TopHoleMap
        (Byte8Map.merge
           (fun key v1 v2 ->
             match (v1, v2) with
             | Some v1, Some v2 -> Some (AbsVal.join v1 v2)
             | _, _ -> None)
           m1 m2)
  | Bottom, _ | _, Bottom -> Bottom

let le (c1 : t) (c2 : t) : bool =
  match (c1, c2) with
  | TopHoleMap m1, TopHoleMap m2 ->
      Byte8Map.for_all
        (fun r s2 ->
          let s1 = Byte8Map.find_opt r m1 |> Option.value ~default:AbsVal.Top in
          AbsVal.le s1 s2)
        m2
  | Bottom, _ -> true
  | _, Bottom -> false

let top = TopHoleMap Byte8Map.empty

let process_load (c : t) (ptr : AbsVal.t) : AbsVal.t =
  match c with
  | TopHoleMap m -> (
      match ptr with
      | Flat (SP o) -> Byte8Map.find_opt o m |> Option.value ~default:AbsVal.top
      | _ -> AbsVal.top)
  | Bottom -> AbsVal.bottom

let process_store (c : t) (ptr : AbsVal.t) (v : AbsVal.t) : t =
  match c with
  | TopHoleMap m -> (
      match ptr with Flat (SP o) -> TopHoleMap (Byte8Map.add o v m) | _ -> c)
  | Bottom -> Bottom
