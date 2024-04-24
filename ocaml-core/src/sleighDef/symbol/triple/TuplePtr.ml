type t =
  | Unknown of Int32.t
  | Family of FamilyPtr.t
  | Specific of SpecificPtr.t

let of_purevalue (v : PureValuePtr.t) : t = Family (FamilyPtr.of_purevalue v)
let of_context (v : ContextPtr.t) : t = Family (FamilyPtr.of_context v)
let of_name (v : NamePtr.t) : t = Family (FamilyPtr.of_name v)
let of_valuemap (v : ValueMapPtr.t) : t = Family (FamilyPtr.of_valuemap v)

let of_varnodelist (v : VarNodeListPtr.t) : t =
  Family (FamilyPtr.of_varnodelist v)

let of_end (v : EndPtr.t) : t = Specific (SpecificPtr.of_end v)
let of_operand (v : OperandPtr.t) : t = Specific (SpecificPtr.of_operand v)
let of_epsilon (v : EpsilonPtr.t) : t = Specific (SpecificPtr.of_epsilon v)
let of_varnode (v : VarNodePtr.t) : t = Specific (SpecificPtr.of_varnode v)
let of_start (v : StartPtr.t) : t = Specific (SpecificPtr.of_start v)
let of_next2 (v : Next2Ptr.t) : t = Specific (SpecificPtr.of_next2 v)

let get_id (symbol : t) : Int32.t =
  match symbol with
  | Unknown v -> v
  | Family v -> FamilyPtr.get_id v
  | Specific v -> SpecificPtr.get_id v

let of_int32 (v : Int32.t) : t = Unknown v
