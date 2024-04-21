open StdlibExt
open Notation

type t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  construct : Constructor.t List.t;
  decisiontree : DecisionNode.t;
}

let partition_list (l : 'a List.t) (x : Int.t) : 'a List.t * 'a List.t =
  let a, b, _ =
    List.fold_left
      (fun (a, b, x) e ->
        match x with 0 -> (a, e :: b, 0) | x -> (e :: a, b, x - 1))
      ([], [], x) l
  in
  (a, b)

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* numct = XmlExt.attrib_int xml "numct" in
  let consts, rest =
    partition_list (Xml.children xml) (numct |> Int32.to_int)
  in
  let* construct =
    List.map (fun xml -> Constructor.decode xml sleighInit) consts
    |> ResultExt.join_list
  in
  let* decisiontree =
    match rest with
    | [ xml ] -> DecisionNode.decode xml
    | _ -> Result.error "Expected exactly one decision node"
  in
  {
    name = header.name;
    id = header.id;
    scopeid = header.scopeid;
    construct;
    decisiontree;
  }
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid

let get_constructor (symbol : t) (index : Int32.t) :
    (Constructor.t, String.t) Result.t =
  List.nth_opt symbol.construct (index |> Int32.to_int)
  |> Option.to_result ~none:"Constructor not found"

let resolve (v : t) (walker : ParserWalker.t) :
    (Constructor.t, String.t) Result.t =
  let* constructorId = DecisionNode.resolve v.decisiontree walker in
  List.nth_opt v.construct (constructorId |> Int32.to_int)
  |> Option.to_result ~none:"Constructor not found"
