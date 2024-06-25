type t = {
  sp_boundary : Int64.t * Int64.t;
  sp_diff : Int64.t;
  inputs : RegId.t List.t;
  outputs : RegId.t List.t;
}
[@@deriving sexp, show]
