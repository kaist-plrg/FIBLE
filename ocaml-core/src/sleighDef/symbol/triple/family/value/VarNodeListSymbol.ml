open StdlibExt
open Notation

type t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  pattern : PatternExpression.t;
  varNodeIds : VarNodePtr.t Option.t List.t;
}

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let childs = XmlExt.children xml in
  match childs with
  | pattern :: varNodeIds ->
      let* pattern = PatternExpression.decode pattern sleighInit in
      let varNodeIds =
        varNodeIds
        |> List.map (fun xml ->
               XmlExt.attrib_int xml "id"
               |> Result.map VarNodePtr.of_int32
               |> Result.to_option)
      in
      {
        name = header.name;
        id = header.id;
        scopeid = header.scopeid;
        pattern;
        varNodeIds;
      }
      |> Result.ok
  | _ -> "Invalid number of children" |> Result.error

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid
