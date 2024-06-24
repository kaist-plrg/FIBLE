include Stdlib.Unit

let t_of_sexp = Sexplib.Conv.unit_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_unit
let pp fmt (p : t) = Format.fprintf fmt ""
