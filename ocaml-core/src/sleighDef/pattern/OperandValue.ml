open StdlibExt
open Notation

type t = { index : Int32.t; table : SubtablePtr.t; ctid : ConstructorPtr.t }

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  let tag = XmlExt.tag xml in
  let* _ =
    if String.equal tag "operand_exp" then () |> Result.ok
    else "Not oprerand" |> Result.error
  in
  let* index = XmlExt.attrib_int xml "index" in
  let* tableId = XmlExt.attrib_int xml "table" in
  let* cid = XmlExt.attrib_int xml "ct" in
  {
    index;
    table = SubtablePtr.of_int32 tableId;
    ctid = ConstructorPtr.make cid tableId;
  }
  |> Result.ok

let pp (fmt : Format.formatter) (v : t) =
  Format.fprintf fmt "{ index: %ld; tablePtr: %ld; ctid: %ld }" v.index
    (SubtablePtr.get_id v.table)
    (ConstructorPtr.get_offset v.ctid)
