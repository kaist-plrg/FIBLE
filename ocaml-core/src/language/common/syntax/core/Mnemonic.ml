open Sexplib.Std
open StdlibExt

type t = string [@@deriving sexp]

let pp fmt s = Format.fprintf fmt "%s" s
