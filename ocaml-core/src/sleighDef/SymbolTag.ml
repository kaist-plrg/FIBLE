type t =
  | TUserOp
  | TEpsilon
  | TPureValue
  | TValueMap
  | TName
  | TVarNode
  | TContext
  | TVarNodeList
  | TOperand
  | TStart
  | TEnd
  | TNext2
  | TSubtable

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
