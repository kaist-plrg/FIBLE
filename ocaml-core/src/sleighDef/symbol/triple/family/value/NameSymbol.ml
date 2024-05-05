open StdlibExt
open Notation

type t = TypeDef.name_t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let childs = XmlExt.children xml in
  match childs with
  | pattern :: names ->
      let* pattern = PatternExpression.decode pattern sleighInit in
      let names =
        names
        |> List.map (fun xml ->
               XmlExt.attrib xml "name" |> Result.value ~default:"\t")
      in
      ({
         name = header.name;
         id = header.id;
         scopeid = header.scopeid;
         pattern;
         names;
       }
        : t)
      |> Result.ok
  | _ -> "Invalid number of children" |> Result.error

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid
let get_pattern (symbol : t) : PatternExpression.t = symbol.pattern

let print (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (String.t, String.t) Result.t =
  let* a = PatternExpression.get_value v.pattern walker pinfo in
  let a = a |> Int64.to_int in
  a |> List.nth_opt v.names |> Option.to_result ~none:"index out of bounds"
