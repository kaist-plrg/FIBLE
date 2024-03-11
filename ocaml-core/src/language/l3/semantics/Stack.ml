open Basic
open Basic_collection

type elem_t = Cursor.t * RegId.t list * RegFile.t * Value.t * Loc.t
type t = elem_t list

let pp fmt (v : t) =
  let pp_elem fmt (c, _, _, v, l) =
    Format.fprintf fmt "%a: %a" Cursor.pp c Value.pp v
  in
  Format.fprintf fmt "[%a]" (Format.pp_print_list pp_elem) v
