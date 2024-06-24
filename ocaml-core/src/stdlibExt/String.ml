include Stdlib.String

let t_of_sexp = Sexplib.Conv.string_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_string
let pp fmt v = Format.fprintf fmt "%s" v
