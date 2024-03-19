open Common

type t = { base : RegId.t; offset : Int64.t }

let compare a b =
  let c = RegId.compare a.base b.base in
  if c = 0 then Int64.compare a.offset b.offset else c

let pp fmt a =
  if a.offset = 0L then Format.fprintf fmt "[%a]" RegId.pp a.base
  else Format.fprintf fmt "[%a + %Ld]" RegId.pp a.base a.offset
