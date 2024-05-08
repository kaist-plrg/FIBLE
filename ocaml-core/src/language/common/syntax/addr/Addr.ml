open StdlibExt

type t = Int64Ext.t

let pp fmt t = Format.fprintf fmt "0x%Lx" t

let scan (ic : Scanf.Scanning.in_channel) : t =
  Scanf.bscanf ic "0x%Lx" (fun x -> x)

let get_offset (v : t) : Int64.t = v
let get_addr_size (v : t) : Int.t = 8
let compare = Int64.compare
let succ = Int64.succ
