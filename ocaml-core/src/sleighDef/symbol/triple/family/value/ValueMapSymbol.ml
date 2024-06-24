type t = TypeDef.valuemap_t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let childs = XmlExt.children xml in
  match childs with
  | pattern :: values ->
      let* pattern = PatternExpression.decode pattern sleighInit in
      let* values =
        values
        |> List.map (fun xml -> XmlExt.attrib_int xml "val")
        |> Result.join_list
      in
      ({
         name = header.name;
         id = header.id;
         scopeid = header.scopeid;
         pattern;
         values;
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
  let* a =
    List.nth_opt v.values (Int64.to_int a)
    |> Option.to_result ~none:"Invalid index"
  in
  if Int32.compare a Int32.zero >= 0 then Printf.sprintf "0x%lx" a |> Result.ok
  else Printf.sprintf "-0x%lx" (Int32.neg a) |> Result.ok

let getFixedHandle (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (FixedHandle.t, String.t) Result.t =
  let* a = PatternExpression.get_value v.pattern walker pinfo in
  let* a =
    List.nth_opt v.values (Int64.to_int a)
    |> Option.to_result ~none:"Invalid index"
  in
  FixedHandle.of_constant (Int64.of_int32 a) |> Result.ok
