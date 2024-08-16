include Stdlib.Array

let pp pp_elt fmt arr =
  Format.fprintf fmt "[|";
  for i = 0 to length arr - 1 do
    if i > 0 then Format.fprintf fmt ";@ ";
    pp_elt fmt (Stdlib.Array.get arr i)
  done;
  Format.fprintf fmt "|]"
