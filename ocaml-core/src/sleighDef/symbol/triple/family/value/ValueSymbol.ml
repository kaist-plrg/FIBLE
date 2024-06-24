(*
   - ValueSymbol
      x PureValueSymbol
      x ContextSymbol
      x NameSymbol
      x ValueMapSymbol
      x VarNodeListSymbol
*)

type 'varnode_t poly_t = 'varnode_t TypeDef.value_poly_t
type ptr_t = TypeDef.value_ptr_t
type t = TypeDef.value_t

let of_purevalue (v : PureValueSymbol.t) : 'varnode_t poly_t = PureValue v
let of_context (v : ContextSymbol.t) : 'varnode_t poly_t = Context v
let of_name (v : NameSymbol.t) : 'varnode_t poly_t = Name v
let of_valuemap (v : ValueMapSymbol.t) : 'varnode_t poly_t = ValueMap v

let of_varnodelist (v : 'varnode_t VarNodeListSymbol.poly_t) : 'varnode_t poly_t
    =
  VarNodeList v

let get_name (symbol : 'varnode_t poly_t) : string =
  match symbol with
  | PureValue v -> PureValueSymbol.get_name v
  | Context v -> ContextSymbol.get_name v
  | Name v -> NameSymbol.get_name v
  | ValueMap v -> ValueMapSymbol.get_name v
  | VarNodeList v -> VarNodeListSymbol.get_name v

let get_id (symbol : 'varnode_t poly_t) : Int32.t =
  match symbol with
  | PureValue v -> PureValueSymbol.get_id v
  | Context v -> ContextSymbol.get_id v
  | Name v -> NameSymbol.get_id v
  | ValueMap v -> ValueMapSymbol.get_id v
  | VarNodeList v -> VarNodeListSymbol.get_id v

let get_scopeid (symbol : 'varnode_t poly_t) : Int32.t =
  match symbol with
  | PureValue v -> PureValueSymbol.get_scopeid v
  | Context v -> ContextSymbol.get_scopeid v
  | Name v -> NameSymbol.get_scopeid v
  | ValueMap v -> ValueMapSymbol.get_scopeid v
  | VarNodeList v -> VarNodeListSymbol.get_scopeid v

let get_pattern (symbol : 'varnode_t poly_t) : PatternExpression.t =
  match symbol with
  | PureValue v -> PureValueSymbol.get_pattern v
  | Context v -> ContextSymbol.get_pattern v
  | Name v -> NameSymbol.get_pattern v
  | ValueMap v -> ValueMapSymbol.get_pattern v
  | VarNodeList v -> VarNodeListSymbol.get_pattern v

let print (symbol : 'varnode_t poly_t) (walker : ParserWalker.t)
    (pinfo : PatternInfo.t) : (String.t, String.t) Result.t =
  match symbol with
  | PureValue v -> PureValueSymbol.print v walker pinfo
  | Context v -> ContextSymbol.print v walker pinfo
  | Name v -> NameSymbol.print v walker pinfo
  | ValueMap v -> ValueMapSymbol.print v walker pinfo
  | VarNodeList v -> VarNodeListSymbol.print v walker pinfo

let getFixedHandle (symbol : 'varnode_t poly_t) (walker : ParserWalker.t)
    (pinfo : PatternInfo.t) : (FixedHandle.t, String.t) Result.t =
  match symbol with
  | PureValue v -> PureValueSymbol.getFixedHandle v walker pinfo
  | Context v -> ContextSymbol.getFixedHandle v walker pinfo
  | Name v -> NameSymbol.getFixedHandle v walker pinfo
  | ValueMap v -> ValueMapSymbol.getFixedHandle v walker pinfo
  | VarNodeList v -> VarNodeListSymbol.getFixedHandle v walker pinfo
