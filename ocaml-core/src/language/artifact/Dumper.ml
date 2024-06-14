let dump (a : Data.t) (path : String.t) : unit =
  let oc = open_out path in
  Sexplib.Sexp.output_hum oc (Data.sexp_of_t a);
  close_out oc
