open Sexplib.Std
include Set.Make (Loc)

let t_of_sexp (se : Sexplib.Sexp.t) : t =
  list_of_sexp Loc.t_of_sexp se |> of_list

let sexp_of_t (v : t) : Sexplib.Sexp.t = sexp_of_list Loc.sexp_of_t (elements v)

let pp (fmt : Format.formatter) (s : t) : unit =
  Format.fprintf fmt "{";
  List.pp Loc.pp fmt (elements s);
  Format.fprintf fmt "}"
