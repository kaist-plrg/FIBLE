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

type 'varnode_t poly_t = 'varnode_t TypeDef.family_poly_t
type t = TypeDef.family_t
type ptr_t = TypeDef.family_ptr_t

let of_purevalue (v : PureValueSymbol.t) : 'varnode_t poly_t =
  Value (ValueSymbol.of_purevalue v)

let of_context (v : ContextSymbol.t) : 'varnode_t poly_t =
  Value (ValueSymbol.of_context v)

let of_name (v : NameSymbol.t) : 'varnode_t poly_t =
  Value (ValueSymbol.of_name v)

let of_valuemap (v : ValueMapSymbol.t) : 'varnode_t poly_t =
  Value (ValueSymbol.of_valuemap v)

let of_varnodelist (v : 'varnode_t VarNodeListSymbol.poly_t) : 'varnode_t poly_t
    =
  Value (ValueSymbol.of_varnodelist v)

let get_name (v : 'varnode_t poly_t) : string =
  match v with Value v -> ValueSymbol.get_name v

let get_id (v : 'varnode_t poly_t) : Int32.t =
  match v with Value v -> ValueSymbol.get_id v

let get_scopeid (v : 'varnode_t poly_t) : Int32.t =
  match v with Value v -> ValueSymbol.get_scopeid v

let get_pattern (v : 'varnode_t poly_t) : PatternExpression.t =
  match v with Value v -> ValueSymbol.get_pattern v

let pp_walker (walker : ParserWalker.t) (fmt : Format.formatter)
    (symbol : 'varnode_t poly_t) : Unit.t =
  Format.fprintf fmt "family"

let print (symbol : 'varnode_t poly_t) (walker : ParserWalker.t)
    (pinfo : PatternInfo.t) : (String.t, String.t) Result.t =
  match symbol with Value v -> ValueSymbol.print v walker pinfo
