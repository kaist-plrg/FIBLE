open StdlibExt
open Notation

type t = TypeDef.purevalue_t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* xml_child = XmlExt.single_child xml in
  let* pattern = PatternExpression.decode xml_child sleighInit in
  ({ name = header.name; id = header.id; scopeid = header.scopeid; pattern }
    : t)
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid
