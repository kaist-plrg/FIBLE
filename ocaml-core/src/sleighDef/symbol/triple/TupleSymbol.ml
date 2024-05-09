open StdlibExt
open Notation

type ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) TypeDef.tuple_poly_t

type ptr_t = TypeDef.tuple_ptr_t
type t = TypeDef.tuple_unmapped

let of_purevalue (v : PureValueSymbol.t) :
    ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Family (FamilySymbol.of_purevalue v)

let of_purevalue_fix (v : PureValueSymbol.t) : t =
  Family (FamilySymbol.of_purevalue v)

let of_context (v : ContextSymbol.t) :
    ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Family (FamilySymbol.of_context v)

let of_context_fix (v : ContextSymbol.t) : t =
  Family (FamilySymbol.of_context v)

let of_name (v : NameSymbol.t) :
    ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Family (FamilySymbol.of_name v)

let of_name_fix (v : NameSymbol.t) : t = Family (FamilySymbol.of_name v)

let of_valuemap (v : ValueMapSymbol.t) :
    ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Family (FamilySymbol.of_valuemap v)

let of_valuemap_fix (v : ValueMapSymbol.t) : t =
  Family (FamilySymbol.of_valuemap v)

let of_varnodelist (v : 'varnode_t VarNodeListSymbol.poly_t) :
    ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Family (FamilySymbol.of_varnodelist v)

let of_varnodelist_fix (v : VarNodeSymbol.t VarNodeListSymbol.poly_t) : t =
  Family (FamilySymbol.of_varnodelist v)

let of_end (v : EndSymbol.t) :
    ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Specific (SpecificSymbol.of_end v)

let of_end_fix (v : EndSymbol.t) : t = Specific (SpecificSymbol.of_end v)

let of_operand (v : ('triple_t, 'mapped_t, 'oper_artifact) OperandSymbol.poly_t)
    : ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Specific (SpecificSymbol.of_operand v)

let try_operand (v : ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t) =
  match v with Specific v -> SpecificSymbol.try_operand v | _ -> None

let of_operand_fix (v : (t, 'mapped_t, 'oper_artifact) OperandSymbol.poly_t) : t
    =
  Specific (SpecificSymbol.of_operand v)

let of_epsilon (v : EpsilonSymbol.t) :
    ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Specific (SpecificSymbol.of_epsilon v)

let of_epsilon_fix (v : EpsilonSymbol.t) : t =
  Specific (SpecificSymbol.of_epsilon v)

let of_varnode (v : VarNodeSymbol.t) :
    ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Specific (SpecificSymbol.of_varnode v)

let try_varnode (v : ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t) =
  match v with Specific v -> SpecificSymbol.try_varnode v | _ -> None

let of_varnode_fix (v : VarNodeSymbol.t) : t =
  Specific (SpecificSymbol.of_varnode v)

let of_start (v : StartSymbol.t) :
    ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Specific (SpecificSymbol.of_start v)

let of_start_fix (v : StartSymbol.t) : t = Specific (SpecificSymbol.of_start v)

let of_next2 (v : Next2Symbol.t) :
    ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t =
  Specific (SpecificSymbol.of_next2 v)

let of_next2_fix (v : Next2Symbol.t) : t = Specific (SpecificSymbol.of_next2 v)

let get_name (symbol : ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t)
    : string =
  match symbol with
  | Family v -> FamilySymbol.get_name v
  | Specific v -> SpecificSymbol.get_name v

let get_name_fix (symbol : ('a, 'b) TypeDef.tuple_t) : string =
  match symbol with
  | Family v -> FamilySymbol.get_name v
  | Specific v -> SpecificSymbol.get_name v

let get_id (symbol : ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t) :
    Int32.t =
  match symbol with
  | Family v -> FamilySymbol.get_id v
  | Specific v -> SpecificSymbol.get_id v

let get_id_fix (symbol : ('a, 'b) TypeDef.tuple_t) : Int32.t =
  match symbol with
  | Family v -> FamilySymbol.get_id v
  | Specific v -> SpecificSymbol.get_id v

let get_scopeid
    (symbol : ('varnode_t, 'tuple_t, 'mapped_t, 'oper_artifact) poly_t) :
    Int32.t =
  match symbol with
  | Family v -> FamilySymbol.get_scopeid v
  | Specific v -> SpecificSymbol.get_scopeid v

let get_scopeid_fix (symbol : ('a, 'b) TypeDef.tuple_t) : Int32.t =
  match symbol with
  | Family v -> FamilySymbol.get_scopeid v
  | Specific v -> SpecificSymbol.get_scopeid v
