open Basic

type elem_t = Cursor.t * Loc.t
type t = elem_t List.t

let pp (fmt : Format.formatter) (t : t) : unit =
  let pp_elem_t (fmt : Format.formatter) (t : elem_t) : unit =
    let l1, l2 = t in
    Format.fprintf fmt "(%a, %a)" Cursor.pp l1 Loc.pp l2
  in
  Format.fprintf fmt "[%a]" (Format.pp_print_list pp_elem_t) t
