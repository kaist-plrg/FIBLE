open StdlibExt
open Notation

type t = TypeDef.varnode_t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* space =
    XmlExt.attrib xml "space"
    |> Fun.flip Result.bind (Spaces.get_space_by_name sleighInit.spaces)
  and* offset = XmlExt.attrib_int xml "offset"
  and* size = XmlExt.attrib_int xml "size" in
  ({
     name = header.name;
     id = header.id;
     scopeid = header.scopeid;
     space;
     offset;
     size;
   }
    : t)
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid

let print (v : t) (walker : ParserWalker.t) : (String.t, String.t) Result.t =
  v.name |> Result.ok

let getFixedHandle (v : t) (walker : ParserWalker.t) :
    (FixedHandle.t, String.t) Result.t =
  FixedHandle.of_varnode v.space (Int64.of_int32 v.offset) v.size |> Result.ok
