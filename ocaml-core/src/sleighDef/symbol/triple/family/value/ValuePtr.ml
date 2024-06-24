(*
   - ValuePtr
      x PureValuePtr
      x ContextPtr
      x NamePtr
      x ValueMapPtr
      x VarNodeListPtr
*)

type t =
  | PureValue of PureValuePtr.t
  | Context of ContextPtr.t
  | Name of NamePtr.t
  | ValueMap of ValueMapPtr.t
  | VarNodeList of VarNodeListPtr.t

let of_purevalue (v : PureValuePtr.t) : t = PureValue v
let of_context (v : ContextPtr.t) : t = Context v
let of_name (v : NamePtr.t) : t = Name v
let of_valuemap (v : ValueMapPtr.t) : t = ValueMap v
let of_varnodelist (v : VarNodeListPtr.t) : t = VarNodeList v

let get_id (symbol : t) : Int32.t =
  match symbol with
  | PureValue v -> PureValuePtr.get_id v
  | Context v -> ContextPtr.get_id v
  | Name v -> NamePtr.get_id v
  | ValueMap v -> ValueMapPtr.get_id v
  | VarNodeList v -> VarNodeListPtr.get_id v
