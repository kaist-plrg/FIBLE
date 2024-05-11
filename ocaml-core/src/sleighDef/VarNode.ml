open StdlibExt
open Notation

type t = { space : AddrSpace.t; size : Int32.t; offset : Int64.t }

let make space size offset = { space; size; offset }

let pp fmt { space; size; offset } =
  Format.fprintf fmt "%a[%Lx]" AddrSpace.pp space offset
