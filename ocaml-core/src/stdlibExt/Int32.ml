include Stdlib.Int32

let t_of_sexp = Sexplib.Conv.int32_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_int32
let pp fmt v = Format.fprintf fmt "%lx" v
