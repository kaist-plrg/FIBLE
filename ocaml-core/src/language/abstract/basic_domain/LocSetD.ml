open Sexplib.Std
include SetD.Make (Common.Loc)

let t_of_sexp (se : Sexplib.Sexp.t) =
  list_of_sexp Common.Loc.t_of_sexp se |> of_list

let sexp_of_t (t : t) = sexp_of_list Common.Loc.sexp_of_t (elements t)
let pp fmt t = Format.fprintf fmt "{%a}" (List.pp Common.Loc.pp) (elements t)
