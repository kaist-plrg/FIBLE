open Sexplib.Std

type t = Unique of int32 | Register of int32 [@@deriving sexp]
type t_full = { id : t; offset : int32; width : int32 } [@@deriving sexp]

let width v = v.width

let pp (fmt : Format.formatter) (v : t) =
  match v with
  | Register n -> Format.fprintf fmt "$%ld" n
  | Unique n -> Format.fprintf fmt "$U%lx" n

let pp_full (fmt : Format.formatter) (v : t_full) =
  Format.fprintf fmt "%a(%ld:%ld)" pp v.id v.offset v.width

let compare (a : t) (b : t) =
  match (a, b) with
  | Register a, Register b -> Int32.compare a b
  | Unique a, Unique b -> Int32.compare a b
  | Register _, Unique _ -> -1
  | Unique _, Register _ -> 1
