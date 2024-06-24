open Sexplib.Std

module Inner = struct
  type t = unit [@@deriving sexp, show]
end

include Common.JCallF.Make (CallTarget) (Inner)
