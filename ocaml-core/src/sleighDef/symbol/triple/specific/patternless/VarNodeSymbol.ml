open StdlibExt
open Notation

type t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  space : AddrSpace.t;
  offset : Int32.t;
  size : Int32.t;
}

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* space =
    XmlExt.attrib xml "space"
    |> Fun.flip Result.bind (Spaces.get_space_by_name sleighInit.spaces)
  and* offset = XmlExt.attrib_int xml "offset"
  and* size = XmlExt.attrib_int xml "size" in
  {
    name = header.name;
    id = header.id;
    scopeid = header.scopeid;
    space;
    offset;
    size;
  }
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid
