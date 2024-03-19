type t = Addr.t * int

let compare (a : t) (b : t) = compare a b
let pp fmt (addr, size) = Format.fprintf fmt "%a:%d" Addr.pp addr size

let scan (ic : Scanf.Scanning.in_channel) : t =
  Scanf.bscanf ic "%r:%d" Addr.scan (fun addr size -> (addr, size))

let to_addr = fst
