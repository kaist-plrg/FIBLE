open Common

type t = { base : RegId.t_full; offset : Z.t }

let compare a b =
  let c = RegId.compare_full a.base b.base in
  if c = 0 then Z.compare a.offset b.offset else c

let pp fmt a =
  if a.offset = Z.zero then Format.fprintf fmt "[%a]" RegId.pp_full a.base
  else Format.fprintf fmt "[%a + %a]" RegId.pp_full a.base Z.pp_print a.offset
