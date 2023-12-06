include Map.Make (struct
  type t = Int32.t

  let compare = Int32.compare
end)
