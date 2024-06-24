(*
     - SpecificPtr
       x EndPtr
       x OperandPtr
       - PatternlessPtr
        x EpsilonPtr
        x VarNodePtr
       x StartPtr
       x Next2Ptr

   *)

type t =
  | End of EndPtr.t
  | Operand of OperandPtr.t
  | Patternless of PatternlessPtr.t
  | Start of StartPtr.t
  | Next2 of Next2Ptr.t

let of_end (v : EndPtr.t) : t = End v
let of_operand (v : OperandPtr.t) : t = Operand v

let of_epsilon (v : EpsilonPtr.t) : t =
  Patternless (PatternlessPtr.of_epsilon v)

let of_varnode (v : VarNodePtr.t) : t =
  Patternless (PatternlessPtr.of_varnode v)

let of_start (v : StartPtr.t) : t = Start v
let of_next2 (v : Next2Ptr.t) : t = Next2 v

let get_id (s : t) : Int32.t =
  match s with
  | End v -> EndPtr.get_id v
  | Operand v -> OperandPtr.get_id v
  | Patternless v -> PatternlessPtr.get_id v
  | Start v -> StartPtr.get_id v
  | Next2 v -> Next2Ptr.get_id v
