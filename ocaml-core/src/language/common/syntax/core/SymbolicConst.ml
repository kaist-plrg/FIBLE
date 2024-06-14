open Sexplib.Std

type section_t = Instruction | Data | Bss [@@deriving sexp]
type t = { section : section_t; offset : int64 } [@@deriving sexp]

let get_width (_ : t) = 8l

let pp fmt { section; offset } =
  Format.fprintf fmt "%s:%Ld"
    (match section with
    | Instruction -> "instruction"
    | Data -> "data"
    | Bss -> "bss")
    offset
