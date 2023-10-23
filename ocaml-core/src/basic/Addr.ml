open StdlibExt

type t = Int64Ext.t

let pp fmt t = Format.fprintf fmt "0x%Lx" t
let scan (ic: Scanf.Scanning.in_channel): t =
  Scanf.bscanf ic "0x%Lx" (fun x -> x)


let compare = Int64.compare
let succ = Int64.succ
