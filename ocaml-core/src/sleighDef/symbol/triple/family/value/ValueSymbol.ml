open StdlibExt
open Notation

(*
   - ValueSymbol
      x PureValueSymbol
      x ContextSymbol
      x NameSymbol
      x ValueMapSymbol
      x VarNodeListSymbol
*)

type t =
  | PureValue of PureValueSymbol.t
  | Context of ContextSymbol.t
  | Name of NameSymbol.t
  | ValueMap of ValueMapSymbol.t
  | VarNodeList of VarNodeListSymbol.t

let of_purevalue (v : PureValueSymbol.t) : t = PureValue v
let of_context (v : ContextSymbol.t) : t = Context v
let of_name (v : NameSymbol.t) : t = Name v
let of_valuemap (v : ValueMapSymbol.t) : t = ValueMap v
let of_varnodelist (v : VarNodeListSymbol.t) : t = VarNodeList v

let get_name (symbol : t) : string =
  match symbol with
  | PureValue v -> PureValueSymbol.get_name v
  | Context v -> ContextSymbol.get_name v
  | Name v -> NameSymbol.get_name v
  | ValueMap v -> ValueMapSymbol.get_name v
  | VarNodeList v -> VarNodeListSymbol.get_name v

let get_id (symbol : t) : Int32.t =
  match symbol with
  | PureValue v -> PureValueSymbol.get_id v
  | Context v -> ContextSymbol.get_id v
  | Name v -> NameSymbol.get_id v
  | ValueMap v -> ValueMapSymbol.get_id v
  | VarNodeList v -> VarNodeListSymbol.get_id v

let get_scopeid (symbol : t) : Int32.t =
  match symbol with
  | PureValue v -> PureValueSymbol.get_scopeid v
  | Context v -> ContextSymbol.get_scopeid v
  | Name v -> NameSymbol.get_scopeid v
  | ValueMap v -> ValueMapSymbol.get_scopeid v
  | VarNodeList v -> VarNodeListSymbol.get_scopeid v
