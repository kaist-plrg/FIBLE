open Sexplib.Std
open Common

module Inner = struct
  type t = { reserved_stack : int64; sp_diff : int64; returns : VarNode.t list }
  [@@deriving sexp, show]
end

include Common.JTailCallF.Make (CallTarget) (Inner)
