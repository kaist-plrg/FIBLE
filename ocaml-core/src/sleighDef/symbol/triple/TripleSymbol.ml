open StdlibExt
open Notation

(* - TripleSymbol
   - FamilySymbol
    - ValueSymbol
     x PureValueSymbol
     x ContextSymbol
     x NameSymbol
     x ValueMapSymbol
     x VarNodeListSymbol
   - SpecificSymbol
     x EndSymbol
     x OperandSymbol
     - PatternlessSymbol
      x EpsilonSymbol
      x VarNodeSymbol
     x StartSymbol
     x Next2Symbol
   x SubtableSymbol
*)
type t =
  | Family of FamilySymbol.t
  | Specific of SpecificSymbol.t
  | Subtable of SubtableSymbol.t

let of_purevalue (v : PureValueSymbol.t) : t =
  Family (FamilySymbol.of_purevalue v)

let of_context (v : ContextSymbol.t) : t = Family (FamilySymbol.of_context v)
let of_name (v : NameSymbol.t) : t = Family (FamilySymbol.of_name v)
let of_valuemap (v : ValueMapSymbol.t) : t = Family (FamilySymbol.of_valuemap v)

let of_varnodelist (v : VarNodeListSymbol.t) : t =
  Family (FamilySymbol.of_varnodelist v)

let of_end (v : EndSymbol.t) : t = Specific (SpecificSymbol.of_end v)

let of_operand (v : OperandSymbol.t) : t =
  Specific (SpecificSymbol.of_operand v)

let of_epsilon (v : EpsilonSymbol.t) : t =
  Specific (SpecificSymbol.of_epsilon v)

let of_varnode (v : VarNodeSymbol.t) : t =
  Specific (SpecificSymbol.of_varnode v)

let of_start (v : StartSymbol.t) : t = Specific (SpecificSymbol.of_start v)
let of_next2 (v : Next2Symbol.t) : t = Specific (SpecificSymbol.of_next2 v)
let of_subtable (v : SubtableSymbol.t) : t = Subtable v

let get_name (symbol : t) : string =
  match symbol with
  | Family v -> FamilySymbol.get_name v
  | Specific v -> SpecificSymbol.get_name v
  | Subtable v -> SubtableSymbol.get_name v

let get_id (symbol : t) : Int32.t =
  match symbol with
  | Family v -> FamilySymbol.get_id v
  | Specific v -> SpecificSymbol.get_id v
  | Subtable v -> SubtableSymbol.get_id v

let get_scopeid (symbol : t) : Int32.t =
  match symbol with
  | Family v -> FamilySymbol.get_scopeid v
  | Specific v -> SpecificSymbol.get_scopeid v
  | Subtable v -> SubtableSymbol.get_scopeid v
