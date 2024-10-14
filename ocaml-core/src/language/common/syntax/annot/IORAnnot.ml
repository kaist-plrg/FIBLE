type t = {
  reserved_stack : Int64.t;
  sp_diff : Int64.t;
  returns : RegId.t List.t;
}
[@@deriving sexp, show]
