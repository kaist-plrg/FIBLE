type t = Int64.t

let pp fmt (v : t) = Format.fprintf fmt "%Ld" v
let succ (v : t) = Int64.succ v
