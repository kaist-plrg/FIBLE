open Sexplib.Std
open Common

module Inner = struct
  type t = { outputs : RegId.t list; inputs : VarNode.t list }
  [@@deriving sexp, show]
end

include Common.CallTargetF.Make (VarNode) (Inner)
