open Basic

type elem_t = Cursor.t * Value.t * Loc.t
type t = elem_t List.t

let pp (fmt : Format.formatter) (v : t) : unit =
  let pp_elem_t (fmt : Format.formatter) (v : elem_t) : unit =
    let (c, v, loc') : elem_t = v in
    Format.fprintf fmt "(%a, %a, %a)" Cursor.pp c Value.pp v Loc.pp loc'
  in
  Format.fprintf fmt "[%a]" (Format.pp_print_list pp_elem_t) v
