open StdlibExt
open Notation

type ('triple_t, 'mapped_t, 'oper_artifact) poly_t =
  ('triple_t, 'mapped_t, 'oper_artifact) TypeDef.operand_poly_t

type t = TypeDef.operand_unmapped
type disas_t = TypeDef.operand_disas
type handle_t = TypeDef.operand_handle
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
        let* localexp = OperandValue.decode localexp sleighInit in
        let* defexp = OperandExpression.decode defexp sleighInit in
        Ok (localexp, Some defexp)
    | [ localexp ] ->
        let* localexp = OperandValue.decode localexp sleighInit in
        Ok (localexp, None)
    | _ -> Error "Expected exactly one or two children"
  in
  let* (opval : TypeDef.operand_ptr_elem) =
    match (tripleId, defexp) with
    | Some tripleId, Some defexp ->
        Error "Expected either a tripleId or a defexp"
    | Some tripleId, None -> TypeDef.OTriple (Either.left tripleId) |> Result.ok
    | None, Some defexp -> TypeDef.ODefExp defexp |> Result.ok
    | None, None -> Error "Expected either a tripleId or a defexp"
  in
  ({
     name = header.name;
     id = header.id;
     scopeid = header.scopeid;
     operand_value = opval;
     localexp;
     flags;
     hand;
     reloffset;
     offsetbase;
     minimumlength;
     mapped = ();
   }
    : ptr_t)
  |> Result.ok

let get_name (symbol : ('a, 'b, 'c) poly_t) : String.t = symbol.name
let get_id (symbol : ('a, 'b, 'c) poly_t) : Int32.t = symbol.id
let get_scopeid (symbol : ('a, 'b, 'c) poly_t) : Int32.t = symbol.scopeid
