open StdlibExt
open Notation

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

type t = Triple of TripleSymbol.t | UserOp of UserOpSymbol.t

let of_userop (v : UserOpSymbol.t) = UserOp v
let of_epsilon (v : EpsilonSymbol.t) = Triple (TripleSymbol.of_epsilon v)
let of_purevalue (v : PureValueSymbol.t) = Triple (TripleSymbol.of_purevalue v)
let of_valuemap (v : ValueMapSymbol.t) = Triple (TripleSymbol.of_valuemap v)
let of_name (v : NameSymbol.t) = Triple (TripleSymbol.of_name v)
let of_varnode (v : VarNodeSymbol.t) = Triple (TripleSymbol.of_varnode v)
let of_context (v : ContextSymbol.t) = Triple (TripleSymbol.of_context v)

let of_varnodelist (v : VarNodeListSymbol.t) =
  Triple (TripleSymbol.of_varnodelist v)

let of_operand (v : OperandSymbol.t) = Triple (TripleSymbol.of_operand v)
let of_start (v : StartSymbol.t) = Triple (TripleSymbol.of_start v)
let of_end (v : EndSymbol.t) = Triple (TripleSymbol.of_end v)
let of_next2 (v : Next2Symbol.t) = Triple (TripleSymbol.of_next2 v)
let of_subtable (v : SubtableSymbol.t) = Triple (TripleSymbol.of_subtable v)

let decode (xml : Xml.xml) (symbolHeaderMap : SymbolHeader.t Int32Map.t)
    (sleighInit : SleighInit.t) : (t, String.t) Result.t =
  let* id = XmlExt.attrib_int xml "id" in
  let* header =
    Int32Map.find_opt id symbolHeaderMap
    |> Option.to_result ~none:"Symbol header not found"
  in
  match header.tag with
  | TUserOp -> UserOpSymbol.decode xml sleighInit header |> Result.map of_userop
  | TEpsilon ->
      EpsilonSymbol.decode xml sleighInit header |> Result.map of_epsilon
  | TPureValue ->
      PureValueSymbol.decode xml sleighInit header |> Result.map of_purevalue
  | TValueMap ->
      ValueMapSymbol.decode xml sleighInit header |> Result.map of_valuemap
  | TName -> NameSymbol.decode xml sleighInit header |> Result.map of_name
  | TVarNode ->
      VarNodeSymbol.decode xml sleighInit header |> Result.map of_varnode
  | TContext ->
      ContextSymbol.decode xml sleighInit header |> Result.map of_context
  | TVarNodeList ->
      VarNodeListSymbol.decode xml sleighInit header
      |> Result.map of_varnodelist
  | TOperand ->
      OperandSymbol.decode xml sleighInit header |> Result.map of_operand
  | TStart -> StartSymbol.decode xml sleighInit header |> Result.map of_start
  | TEnd -> EndSymbol.decode xml sleighInit header |> Result.map of_end
  | TNext2 -> Next2Symbol.decode xml sleighInit header |> Result.map of_next2
  | TSubtable ->
      SubtableSymbol.decode xml sleighInit header |> Result.map of_subtable

let get_name (symbol : t) : String.t =
  match symbol with
  | Triple symbol -> TripleSymbol.get_name symbol
  | UserOp symbol -> UserOpSymbol.get_name symbol

let get_id (symbol : t) : Int32.t =
  match symbol with
  | Triple symbol -> TripleSymbol.get_id symbol
  | UserOp symbol -> UserOpSymbol.get_id symbol

let get_scopeid (symbol : t) : Int32.t =
  match symbol with
  | Triple symbol -> TripleSymbol.get_scopeid symbol
  | UserOp symbol -> UserOpSymbol.get_scopeid symbol
