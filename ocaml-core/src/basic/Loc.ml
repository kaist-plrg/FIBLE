type t = Addr.t * int

let compare = compare
let pp fmt (addr, size) = Format.fprintf fmt "%a:%d" Addr.pp addr size
let to_addr = fst
