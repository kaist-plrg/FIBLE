open Sexplib.Std
open Common

module Inner = struct
  type t = VarNode.t list [@@deriving sexp, show]
end

include Common.JRetF.Make (Inner)
