include Map.Make (struct
  type t = int64

  let compare = Int64.compare
end)

let t_of_sexp f v =
  Sexplib.Conv.list_of_sexp
    (function
      | List [ k; v ] -> (Sexplib.Conv.int64_of_sexp k, f v)
      | _ -> Sexplib.Conv_error.no_variant_match ())
    v
  |> of_list

let sexp_of_t f v =
  Sexplib.Conv.sexp_of_list
    (fun (k, v) -> List [ Sexplib.Conv.sexp_of_int64 k; f v ])
    (bindings v)
