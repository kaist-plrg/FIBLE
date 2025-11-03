include Stdlib.String

let t_of_sexp = Sexplib.Conv.string_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_string
let pp fmt v = Format.fprintf fmt "%s" v

let make_identifier (s : t) : t =
  "M" ^ s |> to_seq
  |> Seq.filter (fun c ->
         (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
         || ('0' <= c && c <= '9')
         || c = '_')
  |> of_seq
