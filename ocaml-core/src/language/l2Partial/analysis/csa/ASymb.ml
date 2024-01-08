open StdlibExt
open Basic

type t =
  | Top
  | ARegId of (RegId.t * Int64Set.t)
  | AStackOffset of (Int64.t * Int64Set.t)
  | Bottom

let join a1 a2 =
  match (a1, a2) with
  | Top, _ | _, Top -> Top
  | Bottom, a | a, Bottom -> a
  | ARegId (r1, s1), ARegId (r2, s2) ->
      if RegId.compare r1 r2 = 0 then ARegId (r1, Int64Set.union s1 s2) else Top
  | AStackOffset (o1, s1), AStackOffset (o2, s2) ->
      if Int64.equal o1 o2 then AStackOffset (o1, Int64Set.union s1 s2) else Top
  | _ -> Top

let le a1 a2 =
  match (a1, a2) with
  | _, Top -> true
  | Top, _ -> false
  | Bottom, _ -> true
  | _, Bottom -> false
  | ARegId (r1, s1), ARegId (r2, s2) ->
      if RegId.compare r1 r2 = 0 then Int64Set.subset s1 s2 else false
  | AStackOffset (o1, s1), AStackOffset (o2, s2) ->
      if Int64.equal o1 o2 then Int64Set.subset s1 s2 else false
  | _ -> false
