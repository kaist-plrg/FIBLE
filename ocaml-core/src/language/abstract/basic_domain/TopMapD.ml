module Make (Ord : Map.OrderedType) (A : DomainSpec.JoinSemiLatitceWithTop) =
struct
  module XMap = Map.Make (Ord)
  include XMap

  type t = A.t XMap.t

  let add (k : key) (v : A.t) (a : t) : t =
    if A.le A.top v then XMap.remove k a else XMap.add k v a

  let update (k : key) (f : A.t option -> A.t option) (a : t) : t =
    XMap.update k
      (fun v ->
        match f v with
        | Some v -> if A.le A.top v then None else Some v
        | None -> None)
      a

  let join (a : t) (b : t) =
    merge
      (fun _ a b ->
        match (a, b) with
        | Some a, Some b ->
            let v = A.join a b in
            if A.le A.top v then None else Some v
        | _, _ -> None)
      a b

  let top = empty

  let le (m1 : t) (m2 : t) : bool =
    fold
      (fun k v2 acc ->
        match find_opt k m1 with
        | Some v1 -> acc && A.le v1 v2
        | None -> acc && A.le A.top v2)
      m2 true
end

module MakeLatticeWithTop
    (Ord : Map.OrderedType)
    (A : DomainSpec.LatticeWithTop) =
struct
  include Make (Ord) (A)

  let meet (a : t) (b : t) =
    merge
      (fun _ a b ->
        match (a, b) with
        | Some a, Some b -> Some (A.meet a b)
        | Some a, None -> Some a
        | None, Some b -> Some b
        | None, None -> None)
      a b
end

module Make_Mut (Ord : Map.OrderedType) (A : DomainSpec.JoinSemiLatitceWithTop) =
struct
  module XMap = MutMap.Make (Ord)
  include XMap

  type t = A.t XMap.t

  let join (a : t) (b : t) : unit =
    merge
      (fun _ a b ->
        match (a, b) with Some a, Some b -> Some (A.join a b) | _, _ -> None)
      a b

  let empty : t = create_empty ()
  let top : t = empty

  let le (m1 : t) (m2 : t) : bool =
    fold
      (fun k v2 acc ->
        match find_opt k m1 with
        | Some v1 -> acc && A.le v1 v2
        | None -> acc && A.le A.top v2)
      m2 true
end
