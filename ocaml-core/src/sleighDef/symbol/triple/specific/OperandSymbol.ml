open StdlibExt
open Notation

type 'triple_t poly_t = 'triple_t TypeDef.operand_poly_t
type t = TypeDef.operand_t
type ptr_t = TypeDef.operand_ptr_t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (ptr_t, String.t) Result.t =
  let* hand = XmlExt.attrib_int xml "index"
  and* reloffset = XmlExt.attrib_int xml "off"
  and* offsetbase = XmlExt.attrib_int xml "base"
  and* minimumlength = XmlExt.attrib_int xml "minlen" in
  let tripleId =
    XmlExt.attrib_int xml "subsym"
    |> Result.map (fun id -> TuplePtr.Unknown id |> Option.some)
    |> Result.value ~default:None
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
  ({
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
    : ptr_t)
  |> Result.ok

let get_name (symbol : 'a poly_t) : String.t = symbol.name
let get_id (symbol : 'a poly_t) : Int32.t = symbol.id
let get_scopeid (symbol : 'a poly_t) : Int32.t = symbol.scopeid
