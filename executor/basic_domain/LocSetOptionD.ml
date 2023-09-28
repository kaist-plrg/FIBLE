type t = LocSetD.t option
let join (l1: t) (l2: t): t = match l1, l2 with
  | Some l1, Some l2 -> Some (LocSetD.union l1 l2)
  | Some l1, None -> Some l1
  | None, Some l2 -> Some l2
  | None, None -> None

let le (l1: t) (l2: t): bool = match l1, l2 with
  | Some l1, Some l2 -> LocSetD.subset l1 l2
  | Some _, None -> false
  | None, Some _ -> true
  | None, None -> true
