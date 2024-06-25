module Make (VarNode : VarNodeF.S) = struct
  type t = VarNode.t List.t [@@deriving sexp, show]
end
