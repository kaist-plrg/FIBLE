open StdlibExt
open Notation

(*
 - FamilySymbol
    - ValueSymbol
     x PureValueSymbol
     x ContextSymbol
     x NameSymbol
     x ValueMapSymbol
     x VarNodeListSymbol
*)

type t = Value of ValueSymbol.t

let of_purevalue (v : PureValueSymbol.t) : t =
  Value (ValueSymbol.of_purevalue v)

let of_context (v : ContextSymbol.t) : t = Value (ValueSymbol.of_context v)
let of_name (v : NameSymbol.t) : t = Value (ValueSymbol.of_name v)
let of_valuemap (v : ValueMapSymbol.t) : t = Value (ValueSymbol.of_valuemap v)

let of_varnodelist (v : VarNodeListSymbol.t) : t =
  Value (ValueSymbol.of_varnodelist v)

let get_name (v : t) : string = match v with Value v -> ValueSymbol.get_name v
let get_id (v : t) : Int32.t = match v with Value v -> ValueSymbol.get_id v

let get_scopeid (v : t) : Int32.t =
  match v with Value v -> ValueSymbol.get_scopeid v
