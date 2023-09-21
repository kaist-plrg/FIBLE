open PCode;;

module LocBotMap = struct
  module Inner = Map.Make(struct
  type t = loc
  let compare = compare
  end)
  include Inner
  let mapjoin (join: 'a -> 'a -> 'a) (m1: 'a t) (m2: 'a t): 'a t =
    merge (fun _ l1 l2 -> match l1, l2 with
      | Some l1, Some l2 -> Some (join l1 l2)
      | Some l1, None -> Some l1
      | None, Some l2 -> Some l2
      | None, None -> None
    ) m1 m2

    let mapole (ole: 'a -> 'a -> bool) (m1: 'a t) (m2: 'a t): bool =
     fold (fun k v acc -> match find_opt k m2 with
      | Some v2 -> acc && (ole v v2)
      | None -> false
     ) m1 true
end

module LocSet = struct
  module Inner = Set.Make(struct
  type t = loc
  let compare = compare
  end)
  include Inner
  let join (l1: t) (l2: t): t = union l1 l2

  let ole (l1: t) (l2: t): bool = subset l1 l2
end

module LocSetOption = struct
  type t = LocSet.t option
  let join (l1: t) (l2: t): t = match l1, l2 with
    | Some l1, Some l2 -> Some (LocSet.union l1 l2)
    | Some l1, None -> Some l1
    | None, Some l2 -> Some l2
    | None, None -> None
  
  let ole (l1: t) (l2: t): bool = match l1, l2 with
    | Some l1, Some l2 -> LocSet.subset l1 l2
    | Some l1, None -> false
    | None, Some l2 -> true
    | None, None -> true
end

module LocHash =
 struct
  type t = loc
  let equal = (=)
  let hash = Hashtbl.hash
 end

module LocHashtbl = struct
  module Inner = Hashtbl.Make(LocHash)
  include Inner
  let update (h: 'a t) (k: loc) (f: 'a option -> 'a option): unit =
    match find_opt h k with
    | Some v -> f (Some v) |> Option.iter (fun v -> replace h k v)
    | None -> f None |> Option.iter (fun v -> add h k v)
end

module Int64Map = struct
  module Inner = Map.Make(struct
  type t = int64
  let compare = compare
  end)
  include Inner
end


module Int64Hash =
  struct
    type t = int64
    let equal = (=)
    let hash = Hashtbl.hash
end

module Int64Hashtbl = struct
  module Inner = Hashtbl.Make(Int64Hash)
  include Inner
  let update (h: 'a t) (k: int64) (f: 'a option -> 'a option): unit =
    match find_opt h k with
    | Some v -> f (Some v) |> Option.iter (fun v -> replace h k v)
    | None -> f None |> Option.iter (fun v -> add h k v)
end