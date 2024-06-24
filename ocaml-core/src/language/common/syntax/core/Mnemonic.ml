open Sexplib.Std

type t = string [@@deriving sexp]

let pp fmt s = Format.fprintf fmt "%s" s
