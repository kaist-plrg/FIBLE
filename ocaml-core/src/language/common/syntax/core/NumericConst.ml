open Sexplib.Std

type t = { value : int64; width : int32 } [@@deriving sexp]

let get_width { width; _ } = width
let pp fmt { value; width } = Format.fprintf fmt "%Lx:%ld" value width
