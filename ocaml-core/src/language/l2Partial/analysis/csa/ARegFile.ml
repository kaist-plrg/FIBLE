open StdlibExt
open Common

type t =
  | TopHoleMap of ASymb.t RegIdMap.t
  | InitHoleMap of ASymb.t RegIdMap.t
  | Bottom

let join (c1 : t) (c2 : t) : t =
  match (c1, c2) with
  | TopHoleMap m1, TopHoleMap m2 ->
      TopHoleMap (RegIdMap.union (fun _ s1 s2 -> Some (ASymb.join s1 s2)) m1 m2)
  | InitHoleMap m1, InitHoleMap m2 ->
      InitHoleMap
        (RegIdMap.merge
           (fun key s1 s2 ->
             match (s1, s2) with
             | Some v1, Some v2 -> Some (ASymb.join v1 v2)
             | Some v, None ->
                 Some (ASymb.join v (ASymb.ARegId (key, Int64Set.empty)))
             | None, Some v ->
                 Some (ASymb.join (ASymb.ARegId (key, Int64Set.empty)) v)
             | None, None -> None)
           m1 m2)
  | TopHoleMap m, InitHoleMap m' | InitHoleMap m', TopHoleMap m ->
      let m'' =
        RegIdMap.merge
          (fun key s1 s2 ->
            match (s1, s2) with
            | Some v1, Some v2 -> Some (ASymb.join v1 v2)
            | Some v, None ->
                Some (ASymb.join v (ASymb.ARegId (key, Int64Set.empty)))
            | None, Some v -> Some ASymb.Top
            | None, None -> None)
          m m'
      in
      TopHoleMap m''
  | Bottom, _ | _, Bottom -> Bottom

let le (c1 : t) (c2 : t) : bool =
  match (c1, c2) with
  | TopHoleMap m1, TopHoleMap m2 ->
      RegIdMap.for_all
        (fun r s2 ->
          let s1 = RegIdMap.find_opt r m1 |> Option.value ~default:ASymb.Top in
          ASymb.le s1 s2)
        m2
  | InitHoleMap m1, InitHoleMap m2 ->
      RegIdMap.for_all
        (fun r s1 ->
          let s2 =
            RegIdMap.find_opt r m2
            |> Option.value ~default:(ASymb.ARegId (r, Int64Set.empty))
          in
          ASymb.le s1 s2)
        m1
      && RegIdMap.for_all
           (fun r s2 ->
             let s1 =
               RegIdMap.find_opt r m1
               |> Option.value ~default:(ASymb.ARegId (r, Int64Set.empty))
             in
             ASymb.le s1 s2)
           m2
  | TopHoleMap m, InitHoleMap m' -> false
  | InitHoleMap m, TopHoleMap m' ->
      RegIdMap.for_all
        (fun r s2 ->
          let s1 =
            RegIdMap.find_opt r m
            |> Option.value ~default:(ASymb.ARegId (r, Int64Set.empty))
          in
          ASymb.le s1 s2)
        m'
  | Bottom, _ -> true
  | _, Bottom -> false

let add (c : t) (r : RegId.t) (s : ASymb.t) : t =
  match c with
  | TopHoleMap m -> TopHoleMap (RegIdMap.add r s m)
  | InitHoleMap m -> InitHoleMap (RegIdMap.add r s m)
  | Bottom -> Bottom

let get (c : t) (r : RegId.t) : ASymb.t =
  match c with
  | TopHoleMap m -> RegIdMap.find_opt r m |> Option.value ~default:ASymb.Top
  | InitHoleMap m ->
      RegIdMap.find_opt r m
      |> Option.value ~default:(ASymb.ARegId (r, Int64Set.empty))
  | Bottom -> ASymb.Bottom

let top = TopHoleMap RegIdMap.empty
