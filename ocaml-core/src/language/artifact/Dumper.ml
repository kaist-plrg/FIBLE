let dump (a : Data.t) (path : String.t) : unit =
  let oc = open_out path in
  Sexplib.Sexp.output_hum oc (Data.sexp_of_t a);
  close_out oc

let write_pp (a : Data.t) (path : String.t) : unit =
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@.%!" Data.pp a;
  close_out oc
