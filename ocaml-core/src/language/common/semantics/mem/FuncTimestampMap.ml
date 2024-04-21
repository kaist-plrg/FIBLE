include Map.Make (struct
  type t = Loc.t * Int64.t

  let compare = compare
end)
