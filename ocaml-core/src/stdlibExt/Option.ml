include Stdlib.Option

let t_of_sexp = Sexplib.Conv.option_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_option

let pp (pp_a : Format.formatter -> 'a -> unit) fmt v =
  match v with
  | None -> Format.fprintf fmt "None"
  | Some v -> Format.fprintf fmt "Some(%a)" pp_a v
