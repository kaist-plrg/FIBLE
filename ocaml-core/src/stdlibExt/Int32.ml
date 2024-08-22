include Stdlib.Int32

let t_of_sexp = Sexplib.Conv.int32_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_int32

let t_of_sexp_hex (s : Sexplib.Sexp.t) : t =
  match s with
  | Sexplib.Sexp.Atom s -> Scanf.sscanf s "%lx" Fun.id
  | _ -> Sexplib.Conv_error.no_variant_match ()

let sexp_of_t_hex (v : t) : Sexplib.Sexp.t =
  Sexplib.Sexp.Atom (Format.sprintf "%lx" v)

let pp fmt v = Format.fprintf fmt "%lx" v
