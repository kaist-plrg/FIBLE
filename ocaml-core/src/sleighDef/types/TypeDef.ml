open StdlibExt
open Notation

type printpiece = Str of String.t | OperInd of Int32.t

type 'operand_t constructor_poly_t = {
  parentId : SubtablePtr.t;
  minimumlength : Int32.t;
  firstWhitespace : Int32.t;
  flowthruIndex : Int32.t Option.t;
  operandIds : 'operand_t List.t;
  printpieces : printpiece List.t;
  context : ContextChange.t List.t;
  tmpl : ConstructTpl.t;
  namedtmpl : ConstructTpl.t Int32Map.t;
}

type constructor_ptr_t = OperandPtr.t constructor_poly_t

(*
   Symbol
    - TripleSymbol
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
    x UserOpSymbol
*)

type user_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  index : Int32.t;
}

type purevalue_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  pattern : PatternExpression.t;
}

type context_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  pattern : PatternExpression.t;
  varNodeId : VarNodePtr.t;
  low : Int32.t;
  high : Int32.t;
  flow : Bool.t;
}

type name_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  pattern : PatternExpression.t;
  names : String.t List.t;
}

type valuemap_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  pattern : PatternExpression.t;
  values : Int32.t List.t;
}

type 'varnode_t varnodelist_poly_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  pattern : PatternExpression.t;
  varNodeIds : 'varnode_t Option.t List.t;
}

type varnodelist_ptr_t = VarNodePtr.t varnodelist_poly_t

type 'varnode_t value_poly_t =
  | PureValue of purevalue_t
  | Context of context_t
  | Name of name_t
  | ValueMap of valuemap_t
  | VarNodeList of 'varnode_t varnodelist_poly_t

type value_ptr_t = VarNodePtr.t value_poly_t
type 'varnode_t family_poly_t = Value of 'varnode_t value_poly_t
type family_ptr_t = VarNodePtr.t family_poly_t

type end_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  const_space : AddrSpace.t;
  patexp : PatternExpression.t;
}

type 'tuple_t operand_poly_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  defexp : PatternExpression.t Option.t;
  tripleId : 'tuple_t Option.t;
  localexp : PatternExpression.t;
  flags : Int32.t;
  hand : Int32.t;
  reloffset : Int32.t;
  offsetbase : Int32.t;
  minimumlength : Int32.t;
}

type operand_ptr_t = TuplePtr.t operand_poly_t

type epsilon_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  const_space : AddrSpace.t;
}

type varnode_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  space : AddrSpace.t;
  offset : Int32.t;
  size : Int32.t;
}

type patternless_t = Epsilon of epsilon_t | VarNode of varnode_t

type start_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  const_space : AddrSpace.t;
  patexp : PatternExpression.t;
}

type next2_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  const_space : AddrSpace.t;
  patexp : PatternExpression.t;
}

type 'tuple_t specific_poly_t =
  | End of end_t
  | Operand of 'tuple_t operand_poly_t
  | Patternless of patternless_t
  | Start of start_t
  | Next2 of next2_t

type specific_ptr_t = TuplePtr.t specific_poly_t

type 'operand_t constructor_map_poly_t = {
  id : Int32.t;
  map : 'operand_t constructor_poly_t Int32Map.t;
}

type constructor_map_ptr_t = OperandPtr.t constructor_map_poly_t

type decision_node_t = {
  num : Int32.t;
  contextdecision : Bool.t;
  startbit : Int32.t;
  bitsize : Int32.t;
}

type 'a decision_poly_t =
  | Leaf of decision_node_t * (DisjointPattern.t * 'a) List.t
  | Node of decision_node_t * 'a decision_poly_t List.t

type decision_ptr_t = ConstructorPtr.t decision_poly_t

type 'operand_t decision_middle_t =
  'operand_t constructor_poly_t decision_poly_t

type ('operand_t, 'constructor_t) subtable_poly_t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  construct : 'operand_t constructor_map_poly_t;
  decisiontree : 'constructor_t decision_poly_t;
}

type 'operand_t subtable_middle_t =
  ('operand_t, 'operand_t constructor_poly_t) subtable_poly_t

type subtable_ptr_t = (OperandPtr.t, ConstructorPtr.t) subtable_poly_t

type ('varnode_t, 'tuple_t) tuple_poly_t =
  | Family of 'varnode_t family_poly_t
  | Specific of 'tuple_t specific_poly_t

type tuple_ptr_t = (VarNodePtr.t, TuplePtr.t) tuple_poly_t

type ('varnode_t, 'tuple_t, 'operand_t, 'constructor_t) triple_poly_t =
  | Tuple of ('varnode_t, 'tuple_t) tuple_poly_t
  | Subtable of ('operand_t, 'constructor_t) subtable_poly_t

type triple_ptr_t =
  (VarNodePtr.t, TuplePtr.t, OperandPtr.t, ConstructorPtr.t) triple_poly_t

type ('varnode_t, 'tuple_t, 'operand_t, 'constructor_t) sym_poly_t =
  | Triple of ('varnode_t, 'tuple_t, 'operand_t, 'constructor_t) triple_poly_t
  | UserOp of user_t

type sym_ptr_t =
  (VarNodePtr.t, TuplePtr.t, OperandPtr.t, ConstructorPtr.t) sym_poly_t

(* resolve recursion *)
type constructor_t = operand_t constructor_poly_t
and operand_t = tuple_t operand_poly_t

and tuple_t =
  | Family of varnode_t family_poly_t
  | Specific of tuple_t specific_poly_t

type constructor_map_t = operand_t constructor_map_poly_t
type varnodelist_t = varnode_t varnodelist_poly_t
type value_t = varnode_t value_poly_t
type family_t = varnode_t family_poly_t
type decision_t = constructor_t decision_poly_t
type subtable_t = (operand_t, constructor_t) subtable_poly_t
type specific_t = tuple_t specific_poly_t

type triple_t =
  | Tuple of tuple_t
  | Subtable of (operand_t, constructor_t) subtable_poly_t

type sym_t = (varnode_t, tuple_t, operand_t, constructor_t) sym_poly_t
