(*

       - PatternlessSymbol
        x EpsilonSymbol
        x VarNodeSymbol

        have patexp = ConstantValue(0)

   *)

type t = TypeDef.patternless_t

let of_epsilon (v : EpsilonSymbol.t) : t = Epsilon v
let of_varnode (v : VarNodeSymbol.t) : t = VarNode v

let try_varnode (p : t) : VarNodeSymbol.t option =
  match p with VarNode v -> Some v | _ -> None

let get_name (symbol : t) : string =
  match symbol with
  | Epsilon v -> EpsilonSymbol.get_name v
  | VarNode v -> VarNodeSymbol.get_name v

let get_id (symbol : t) : Int32.t =
  match symbol with
  | Epsilon v -> EpsilonSymbol.get_id v
  | VarNode v -> VarNodeSymbol.get_id v

let get_scopeid (symbol : t) : Int32.t =
  match symbol with
  | Epsilon v -> EpsilonSymbol.get_scopeid v
  | VarNode v -> VarNodeSymbol.get_scopeid v

let print (symbol : t) (walker : ParserWalker.t) : (String.t, String.t) Result.t
    =
  match symbol with
  | Epsilon v -> EpsilonSymbol.print v walker
  | VarNode v -> VarNodeSymbol.print v walker

let getFixedHandle (symbol : t) (walker : ParserWalker.t) :
    (FixedHandle.t, String.t) Result.t =
  match symbol with
  | Epsilon v -> EpsilonSymbol.getFixedHandle v walker
  | VarNode v -> VarNodeSymbol.getFixedHandle v walker
