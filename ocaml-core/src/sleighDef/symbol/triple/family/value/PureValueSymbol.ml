open StdlibExt
open Notation

type t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  pattern : PatternExpression.t;
}

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* xml_child = XmlExt.single_child xml in
  let* pattern = PatternExpression.decode xml_child sleighInit in
  { name = header.name; id = header.id; scopeid = header.scopeid; pattern }
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid
