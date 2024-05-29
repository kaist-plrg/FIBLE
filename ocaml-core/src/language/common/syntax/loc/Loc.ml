type t = Byte8.t * int

let compare (a : t) (b : t) = compare a b
let pp fmt (addr, seq) = Format.fprintf fmt "%a:%d" Byte8.pp addr seq

let scan (ic : Scanf.Scanning.in_channel) : t =
  Scanf.bscanf ic "%r:%d" Byte8.scan (fun addr seq -> (addr, seq))

let of_addr addr = (addr, 0)
let of_addr_seq = Fun.id
let get_addr = fst
let get_seq = snd
let to_addr_seq = Fun.id
