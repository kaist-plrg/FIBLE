open StdlibExt

type t = Int64Ext.t

let pp fmt t = Format.fprintf fmt "0x%Ld" t
