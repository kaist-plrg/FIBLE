module Make (A : Set.OrderedType) = struct
  include Set.Make (A)

  let join (l1 : t) (l2 : t) : t = union l1 l2
  let le (l1 : t) (l2 : t) : bool = subset l1 l2
end

module Make_Mut (A : Set.OrderedType) = struct
  include MutSet.Make (A)

  let join (l1 : t) (l2 : t) : unit = union l1 l2
  let le (l1 : t) (l2 : t) : bool = subset l1 l2
end
