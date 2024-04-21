open StdlibExt
open Notation

(*
 - FamilyPtr
    - ValuePtr
     x PureValuePtr
     x ContextPtr
     x NamePtr
     x ValueMapPtr
     x VarNodeListPtr
*)

type t = Value of ValuePtr.t

let of_purevalue (v : PureValuePtr.t) : t = Value (ValuePtr.of_purevalue v)
let of_context (v : ContextPtr.t) : t = Value (ValuePtr.of_context v)
let of_name (v : NamePtr.t) : t = Value (ValuePtr.of_name v)
let of_valuemap (v : ValueMapPtr.t) : t = Value (ValuePtr.of_valuemap v)

let of_varnodelist (v : VarNodeListPtr.t) : t =
  Value (ValuePtr.of_varnodelist v)

let get_id (v : t) : Int32.t = match v with Value v -> ValuePtr.get_id v
