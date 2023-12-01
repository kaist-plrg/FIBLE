type t = Key.t * Key.t

let pp fmt (a : t) = Format.fprintf fmt "(%a, %a)" Key.pp (fst a) Key.pp (snd a)
let compare = compare
