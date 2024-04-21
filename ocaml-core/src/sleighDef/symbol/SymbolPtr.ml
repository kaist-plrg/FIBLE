open StdlibExt
open Notation

(*
   Symbol
    - TripleSymbol
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
    x UserOpSymbol
*)

type t = TriplePtr of TriplePtr.t | UserOpPtr of UserOpPtr.t

let of_userop (v : UserOpPtr.t) = UserOpPtr v
let of_epsilon (v : EpsilonPtr.t) = TriplePtr (TriplePtr.of_epsilon v)
let of_purevalue (v : PureValuePtr.t) = TriplePtr (TriplePtr.of_purevalue v)
let of_valuemap (v : ValueMapPtr.t) = TriplePtr (TriplePtr.of_valuemap v)
let of_name (v : NamePtr.t) = TriplePtr (TriplePtr.of_name v)
let of_varnode (v : VarNodePtr.t) = TriplePtr (TriplePtr.of_varnode v)
let of_context (v : ContextPtr.t) = TriplePtr (TriplePtr.of_context v)

let of_varnodelist (v : VarNodeListPtr.t) =
  TriplePtr (TriplePtr.of_varnodelist v)

let of_operand (v : OperandPtr.t) = TriplePtr (TriplePtr.of_operand v)
let of_start (v : StartPtr.t) = TriplePtr (TriplePtr.of_start v)
let of_end (v : EndPtr.t) = TriplePtr (TriplePtr.of_end v)
let of_next2 (v : Next2Ptr.t) = TriplePtr (TriplePtr.of_next2 v)
let of_subtable (v : SubtablePtr.t) = TriplePtr (TriplePtr.of_subtable v)

let get_id (symbol : t) : Int32.t =
  match symbol with
  | TriplePtr symbol -> TriplePtr.get_id symbol
  | UserOpPtr symbol -> UserOpPtr.get_id symbol
