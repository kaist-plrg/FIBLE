type t = Key.t * Key.t

let pp fmt (a : t) = Format.fprintf fmt "(%a, %a)" Key.pp (fst a) Key.pp (snd a)

let compare (a : t) (b : t) =
  let c = Key.compare (fst a) (fst b) in
  if c = 0 then Key.compare (snd a) (snd b) else c
