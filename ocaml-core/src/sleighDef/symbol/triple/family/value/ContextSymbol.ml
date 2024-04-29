open StdlibExt
open Notation

type t = TypeDef.context_t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* child = XmlExt.single_child xml in
  let* pattern = PatternExpression.decode child sleighInit
  and* varNodeId =
    XmlExt.attrib_int xml "varnode" |> Result.map VarNodePtr.of_int32
  and* low = XmlExt.attrib_int xml "low"
  and* high = XmlExt.attrib_int xml "high" in
  let flow = XmlExt.attrib_bool_value xml "flow" false in
  ({
     name = header.name;
     id = header.id;
     scopeid = header.scopeid;
     pattern;
     varNodeId;
     low;
     high;
     flow;
   }
    : t)
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid
let get_pattern (symbol : t) : PatternExpression.t = symbol.pattern

let print (v : t) (walker : ParserWalker.t) : (String.t, String.t) Result.t =
  PatternExpression.get_value_string v.pattern walker
