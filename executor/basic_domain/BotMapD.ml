open StdlibExt;;

module Make (Ord: Map.OrderedType) (A: DomainSpec.JoinSemiLatitce) = struct
  module XMap = Map.Make(Ord)
  include XMap
  type t = A.t XMap.t
  let join (m1: t) (m2: t): t =
    merge (fun _ l1 l2 -> match l1, l2 with
      | Some l1, Some l2 -> Some (A.join l1 l2)
      | Some l1, None -> Some l1
      | None, Some l2 -> Some l2
      | None, None -> None
    ) m1 m2

    let le (m1: t) (m2: t): bool =
     fold (fun k v acc -> match find_opt k m2 with
      | Some v2 -> acc && (A.le v v2)
      | None -> false
     ) m1 true
end

module Make_Mut (Ord: Map.OrderedType) (A: DomainSpec.JoinSemiLatitce) = struct
  module XMap = MutMap.Make(Ord)
  include XMap
  type t = A.t XMap.t
  let join (m1: t) (m2: t): unit =
    merge (fun _ l1 l2 -> match l1, l2 with
      | Some l1, Some l2 -> Some (A.join l1 l2)
      | Some l1, None -> Some l1
      | None, Some l2 -> Some l2
      | None, None -> None
    ) m1 m2

    let le (m1: t) (m2: t): bool =
     fold (fun k v acc -> match find_opt k m2 with
      | Some v2 -> acc && (A.le v v2)
      | None -> false
     ) m1 true
end