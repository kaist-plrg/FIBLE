type section_t = Instruction | Data | Bss
type t = { section : section_t; offset : Int64.t }

let get_width (_ : t) = 8l

let pp fmt { section; offset } =
  Format.fprintf fmt "%s:%Ld"
    (match section with
    | Instruction -> "instruction"
    | Data -> "data"
    | Bss -> "bss")
    offset
