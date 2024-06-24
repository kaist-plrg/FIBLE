(* - TripleSymbol
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
*)
type ('varnode_t,
       'triple_t,
       'mapped_t,
       'operand_t,
       'oper_artifact,
       'constructor_t)
     poly_t =
  ( 'varnode_t,
    'triple_t,
    'mapped_t,
    'operand_t,
    'oper_artifact,
    'constructor_t )
  TypeDef.triple_poly_t

type t = TypeDef.triple_unmapped
type ptr_t = TypeDef.triple_ptr_t

let of_purevalue (v : PureValueSymbol.t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_purevalue v)

let of_context (v : ContextSymbol.t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_context v)

let of_name (v : NameSymbol.t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_name v)

let of_valuemap (v : ValueMapSymbol.t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_valuemap v)

let of_varnodelist (v : 'varnode_t VarNodeListSymbol.poly_t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_varnodelist v)

let of_end (v : EndSymbol.t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_end v)

let of_operand (v : ('triple_t, 'mapped_t, 'oper_artifact) OperandSymbol.poly_t)
    :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_operand v)

let try_operand
    (v :
      ( 'varnode_t,
        'triple_t,
        'mapped_t,
        'operand_t,
        'oper_artifact,
        'constructor_t )
      poly_t) =
  match v with Tuple v -> TupleSymbol.try_operand v | _ -> None

let of_epsilon (v : EpsilonSymbol.t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_epsilon v)

let of_varnode (v : VarNodeSymbol.t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_varnode v)

let try_varnode
    (v :
      ( 'varnode_t,
        'triple_t,
        'mapped_t,
        'operand_t,
        'oper_artifact,
        'constructor_t )
      poly_t) =
  match v with Tuple v -> TupleSymbol.try_varnode v | _ -> None

let of_start (v : StartSymbol.t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_start v)

let of_next2 (v : Next2Symbol.t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Tuple (TupleSymbol.of_next2 v)

let try_tuple
    (v :
      ( 'varnode_t,
        'triple_t,
        'mapped_t,
        'operand_t,
        'oper_artifact,
        'constructor_t )
      poly_t) =
  match v with Tuple v -> Some v | _ -> None

let of_subtable (v : ('operand_t, 'constructor_t) SubtableSymbol.poly_t) :
    ( 'varnode_t,
      'triple_t,
      'mapped_t,
      'operand_t,
      'oper_artifact,
      'constructor_t )
    poly_t =
  Subtable v

let get_name
    (symbol :
      ( 'varnode_t,
        'triple_t,
        'mapped_t,
        'operand_t,
        'oper_artifact,
        'constructor_t )
      poly_t) : string =
  match symbol with
  | Tuple v -> TupleSymbol.get_name v
  | Subtable v -> SubtableSymbol.get_name v

let get_id
    (symbol :
      ( 'varnode_t,
        'triple_t,
        'mapped_t,
        'operand_t,
        'oper_artifact,
        'constructor_t )
      poly_t) : Int32.t =
  match symbol with
  | Tuple v -> TupleSymbol.get_id v
  | Subtable v -> SubtableSymbol.get_id v

let get_scopeid
    (symbol :
      ( 'varnode_t,
        'triple_t,
        'mapped_t,
        'operand_t,
        'oper_artifact,
        'constructor_t )
      poly_t) : Int32.t =
  match symbol with
  | Tuple v -> TupleSymbol.get_scopeid v
  | Subtable v -> SubtableSymbol.get_scopeid v
