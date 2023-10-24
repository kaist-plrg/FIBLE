include Map.Make (struct
  type t = String.t

  let compare = compare
end)
