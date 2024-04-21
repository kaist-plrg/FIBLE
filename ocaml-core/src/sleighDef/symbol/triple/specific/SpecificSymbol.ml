open StdlibExt
open Notation

(*
     - SpecificSymbol
       x EndSymbol
       x OperandSymbol
       - PatternlessSymbol
        x EpsilonSymbol
        x VarNodeSymbol
       x StartSymbol
       x Next2Symbol

   *)

type t =
  | End of EndSymbol.t
  | Operand of OperandSymbol.t
  | Patternless of PatternlessSymbol.t
  | Start of StartSymbol.t
  | Next2 of Next2Symbol.t

let of_end (v : EndSymbol.t) : t = End v
let of_operand (v : OperandSymbol.t) : t = Operand v

let of_epsilon (v : EpsilonSymbol.t) : t =
  Patternless (PatternlessSymbol.of_epsilon v)

let of_varnode (v : VarNodeSymbol.t) : t =
  Patternless (PatternlessSymbol.of_varnode v)

let of_start (v : StartSymbol.t) : t = Start v
let of_next2 (v : Next2Symbol.t) : t = Next2 v

let get_name (symbol : t) : string =
  match symbol with
  | End v -> EndSymbol.get_name v
  | Operand v -> OperandSymbol.get_name v
  | Patternless v -> PatternlessSymbol.get_name v
  | Start v -> StartSymbol.get_name v
  | Next2 v -> Next2Symbol.get_name v

let get_id (symbol : t) : Int32.t =
  match symbol with
  | End v -> EndSymbol.get_id v
  | Operand v -> OperandSymbol.get_id v
  | Patternless v -> PatternlessSymbol.get_id v
  | Start v -> StartSymbol.get_id v
  | Next2 v -> Next2Symbol.get_id v

let get_scopeid (symbol : t) : Int32.t =
  match symbol with
  | End v -> EndSymbol.get_scopeid v
  | Operand v -> OperandSymbol.get_scopeid v
  | Patternless v -> PatternlessSymbol.get_scopeid v
  | Start v -> StartSymbol.get_scopeid v
  | Next2 v -> Next2Symbol.get_scopeid v
