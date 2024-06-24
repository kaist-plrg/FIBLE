type 'varnode_t poly_t = 'varnode_t TypeDef.varnodelist_poly_t
type t = TypeDef.varnodelist_t
type ptr_t = TypeDef.varnodelist_ptr_t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (ptr_t, String.t) Result.t =
  let childs = XmlExt.children xml in
  match childs with
  | pattern :: varNodeIds ->
      let* pattern = PatternExpression.decode pattern sleighInit in
      let varNodeIds =
        varNodeIds
        |> List.map (fun xml ->
               XmlExt.attrib_int xml "id"
               |> Result.map VarNodePtr.of_int32
               |> Result.to_option)
      in
      ({
         name = header.name;
         id = header.id;
         scopeid = header.scopeid;
         pattern;
         varNodeIds;
       }
        : ptr_t)
      |> Result.ok
  | _ -> "Invalid number of children" |> Result.error

let get_name (symbol : 'varnode_t poly_t) : String.t = symbol.name
let get_id (symbol : 'varnode_t poly_t) : Int32.t = symbol.id
let get_scopeid (symbol : 'varnode_t poly_t) : Int32.t = symbol.scopeid
let get_pattern (symbol : t) : PatternExpression.t = symbol.pattern

let print (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (String.t, String.t) Result.t =
  let* a = PatternExpression.get_value v.pattern walker pinfo in
  let a = a |> Int64.to_int in
  let* varnode =
    a |> List.nth_opt v.varNodeIds
    |> Option.to_result ~none:"index out of bounds"
  in
  let* varnode = varnode |> Option.to_result ~none:"varnode not found" in
  VarNodeSymbol.get_name varnode |> Result.ok

let getFixedHandle (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (FixedHandle.t, String.t) Result.t =
  let* a = PatternExpression.get_value v.pattern walker pinfo in
  let a = a |> Int64.to_int in
  let* varnode =
    a |> List.nth_opt v.varNodeIds
    |> Option.to_result ~none:"index out of bounds"
  in
  let* varnode = varnode |> Option.to_result ~none:"varnode not found" in
  VarNodeSymbol.getFixedHandle varnode walker
