include Set.Make (struct
  type t = int64

  let compare = Int64.compare
end)
