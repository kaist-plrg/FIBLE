open StdlibExt
open Notation

(* - TriplePtr
   - FamilyPtr
    - ValuePtr
     x PureValuePtr
     x ContextPtr
     x NamePtr
     x ValueMapPtr
     x VarNodeListPtr
   - SpecificPtr
     x EndPtr
     x OperandPtr
     - PatternlessPtr
      x EpsilonPtr
      x VarNodePtr
     x StartPtr
     x Next2Ptr
   x SubtablePtr
*)
type t = Tuple of TuplePtr.t | Subtable of SubtablePtr.t

let of_purevalue (v : PureValuePtr.t) : t = Tuple (TuplePtr.of_purevalue v)
let of_context (v : ContextPtr.t) : t = Tuple (TuplePtr.of_context v)
let of_name (v : NamePtr.t) : t = Tuple (TuplePtr.of_name v)
let of_valuemap (v : ValueMapPtr.t) : t = Tuple (TuplePtr.of_valuemap v)

let of_varnodelist (v : VarNodeListPtr.t) : t =
  Tuple (TuplePtr.of_varnodelist v)

let of_end (v : EndPtr.t) : t = Tuple (TuplePtr.of_end v)
let of_operand (v : OperandPtr.t) : t = Tuple (TuplePtr.of_operand v)
let of_epsilon (v : EpsilonPtr.t) : t = Tuple (TuplePtr.of_epsilon v)
let of_varnode (v : VarNodePtr.t) : t = Tuple (TuplePtr.of_varnode v)
let of_start (v : StartPtr.t) : t = Tuple (TuplePtr.of_start v)
let of_next2 (v : Next2Ptr.t) : t = Tuple (TuplePtr.of_next2 v)
let of_subtable (v : SubtablePtr.t) : t = Subtable v

let get_id (symbol : t) : Int32.t =
  match symbol with
  | Tuple v -> TuplePtr.get_id v
  | Subtable v -> SubtablePtr.get_id v
