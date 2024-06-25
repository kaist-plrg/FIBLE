module Make (VarNode : VarNodeF.S) = struct
  type t = { outputs : RegId.t List.t; inputs : VarNode.t List.t }
  [@@deriving sexp, show]
end
