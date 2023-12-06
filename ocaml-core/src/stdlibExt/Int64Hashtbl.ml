include HashtblExt.Make (struct
  type t = int64

  let equal = ( = )
  let hash = Int64.hash
end)
