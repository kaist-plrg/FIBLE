open Sexplib.Std

module Inner = struct
  type t = unit [@@deriving sexp, show]
end

include Common.JTailCallF.Make (CallTarget) (Inner)
