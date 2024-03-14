open Common

type t = Top | Set of RegIdSet.t

let pp fmt v =
  match v with
  | Top -> Format.fprintf fmt "Top"
  | Set s ->
      Format.fprintf fmt "{%a}"
        (Format.pp_print_list RegId.pp ~pp_sep:(fun fmt () ->
             Format.fprintf fmt ","))
        (RegIdSet.elements s)

let join a b =
  match (a, b) with
  | Top, _ | _, Top -> Top
  | Set a, Set b -> Set (RegIdSet.union a b)

let meet a b =
  match (a, b) with
  | Top, x | x, Top -> x
  | Set a, Set b -> Set (RegIdSet.inter a b)

let le a b =
  match (a, b) with
  | _, Top -> true
  | Top, _ -> false
  | Set a, Set b -> RegIdSet.subset a b

let top = Top
let bot = Set RegIdSet.empty
