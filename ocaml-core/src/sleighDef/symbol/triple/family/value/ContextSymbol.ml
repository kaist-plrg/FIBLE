open StdlibExt
open Notation

type t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  pattern : PatternExpression.t;
  varNodeId : VarNodePtr.t;
  low : Int32.t;
  high : Int32.t;
  flow : Bool.t;
}

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* child = XmlExt.single_child xml in
  let* pattern = PatternExpression.decode child sleighInit
  and* varNodeId =
    XmlExt.attrib_int xml "varnode" |> Result.map VarNodePtr.of_int32
  and* low = XmlExt.attrib_int xml "low"
  and* high = XmlExt.attrib_int xml "high" in
  let flow = XmlExt.attrib_bool_value xml "flow" false in
  {
    name = header.name;
    id = header.id;
    scopeid = header.scopeid;
    pattern;
    varNodeId;
    low;
    high;
    flow;
  }
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid
