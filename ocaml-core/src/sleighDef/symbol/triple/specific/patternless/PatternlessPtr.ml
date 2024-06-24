(*

       - PatternlessSymbol
        x EpsilonSymbol
        x VarNodeSymbol

        have patexp = ConstantValue(0)

   *)

type t = Epsilon of EpsilonPtr.t | VarNode of VarNodePtr.t

let of_epsilon (v : EpsilonPtr.t) : t = Epsilon v
let of_varnode (v : VarNodePtr.t) : t = VarNode v

let get_id (symbol : t) : Int32.t =
  match symbol with
  | Epsilon v -> EpsilonPtr.get_id v
  | VarNode v -> VarNodePtr.get_id v
