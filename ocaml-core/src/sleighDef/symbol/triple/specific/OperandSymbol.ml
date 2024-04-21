open StdlibExt
open Notation

type t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  defexp : PatternExpression.t Option.t;
  tripleId : Int32.t Option.t;
  localexp : PatternExpression.t;
  flags : Int32.t;
  hand : Int32.t;
  reloffset : Int32.t;
  offsetbase : Int32.t;
  minimumlength : Int32.t;
}

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* hand = XmlExt.attrib_int xml "index"
  and* reloffset = XmlExt.attrib_int xml "off"
  and* offsetbase = XmlExt.attrib_int xml "base"
  and* minimumlength = XmlExt.attrib_int xml "minlen" in
  let tripleId =
    XmlExt.attrib_int xml "subsym"
    |> Result.map Option.some |> Result.value ~default:None
  in
  let flags = if XmlExt.attrib_bool_value xml "code" false then 1l else 0l in
  let* localexp, defexp =
    match XmlExt.children xml with
    | [ localexp; defexp ] ->
        let* localexp = PatternExpression.decode localexp sleighInit in
        let* defexp = PatternExpression.decode defexp sleighInit in
        Ok (localexp, Some defexp)
    | [ localexp ] ->
        let* localexp = PatternExpression.decode localexp sleighInit in
        Ok (localexp, None)
    | _ -> Error "Expected exactly one or two children"
  in
  {
    name = header.name;
    id = header.id;
    scopeid = header.scopeid;
    defexp;
    tripleId;
    localexp;
    flags;
    hand;
    reloffset;
    offsetbase;
    minimumlength;
  }
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid
