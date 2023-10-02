type t = LocSetD_Mut.t option ref

let join (l1 : t) (l2 : t) : unit =
  match (!l1, !l2) with
  | Some l1, Some l2 -> LocSetD_Mut.union l1 l2
  | Some _, None -> ()
  | None, Some l2 -> l1 := Some l2
  | None, None -> ()

let le (l1 : t) (l2 : t) : bool =
  match (!l1, !l2) with
  | Some l1, Some l2 -> LocSetD_Mut.subset l1 l2
  | Some _, None -> false
  | None, Some _ -> true
  | None, None -> true
