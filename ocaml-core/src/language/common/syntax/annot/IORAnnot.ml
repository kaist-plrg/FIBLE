module Make (VarNode : VarNodeF.S) = struct
  type t = {
    reserved_stack : Int64.t;
    sp_diff : Int64.t;
    returns : VarNode.t List.t;
  }
  [@@deriving sexp, show]
end
