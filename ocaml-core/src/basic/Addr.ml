open StdlibExt

type t = Int64Ext.t

let pp fmt t = Format.fprintf fmt "0x%Lx" t
let compare = Int64.compare
let succ = Int64.succ
