open StdlibExt
open Notation

type t = { name : String.t; id : Int32.t; scopeid : Int32.t; index : Int32.t }

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* index = XmlExt.attrib_int xml "index" in
  { name = header.name; id = header.id; scopeid = header.scopeid; index }
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid
