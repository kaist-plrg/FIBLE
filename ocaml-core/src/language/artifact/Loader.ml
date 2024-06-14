let load (path : String.t) : Data.t =
  let ic = open_in path in
  let p = Sexplib.Sexp.input_sexp ic |> Data.t_of_sexp in
  close_in ic;
  p
