open Common
open Basic_domain
open Value_domain
module FinSet = Int64SetD

let cardinal_limit = 1000

type t = Top | Fin of FinSet.t

let top = Top
let bottom = Fin FinSet.empty

let join a b =
  match (a, b) with
  | Top, _ -> Top
  | _, Top -> Top
  | Fin a, Fin b ->
      let c = FinSet.union a b in
      if FinSet.cardinal c > cardinal_limit then Top else Fin c

let meet a b =
  match (a, b) with
  | Top, x -> x
  | x, Top -> x
  | Fin a, Fin b -> Fin (FinSet.inter a b)

let widen a b = join a b

let le a b =
  match (a, b) with
  | Top, _ -> true
  | _, Top -> false
  | Fin a, Fin b -> FinSet.subset a b

let log_access width spv =
  match spv with
  | AbsVal.Flat (AbsVal.Inner.SP x) ->
      Fin (FinSet.of_list [ x; Int64.add x (Int64.of_int32 width) ])
  | _ -> bottom
