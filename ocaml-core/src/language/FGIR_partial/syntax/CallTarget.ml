open Sexplib.Std

module Inner = struct
  type t = unit [@@deriving sexp, show]
end

include Common.CallTargetF.Make (VarNode) (Inner)
