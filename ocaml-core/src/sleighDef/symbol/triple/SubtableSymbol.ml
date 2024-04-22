open StdlibExt
open Notation

type t = {
  name : String.t;
  id : Int32.t;
  scopeid : Int32.t;
  construct : ConstructorMap.t;
  decisiontree : DecisionNode.t;
}

let partition_list (l : 'a List.t) (x : Int.t) : 'a List.t * 'a List.t =
  let a, b, _ =
    List.fold_left
      (fun (a, b, x) e ->
        match x with 0 -> (a, e :: b, 0) | x -> (e :: a, b, x - 1))
      ([], [], x) l
  in
  (a |> List.rev, b |> List.rev)

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
  let constructMap = construct |> Fun.flip ConstructorMap.of_list header.id in
  let* decisiontree =
    match rest with
    | [ xml ] -> DecisionNode.decode xml header.id
    | _ -> Result.error "Expected exactly one decision node"
  in
  {
    name = header.name;
    id = header.id;
    scopeid = header.scopeid;
    construct = constructMap;
    decisiontree;
  }
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid

let get_constructor (symbol : t) (ptr : ConstructorPtr.t) :
    (Constructor.t, String.t) Result.t =
  ConstructorMap.get_constructor symbol.construct ptr

let resolve (v : t) (walker : ParserWalker.t) :
    (Constructor.t, String.t) Result.t =
  let* ptr = DecisionNode.resolve v.decisiontree walker in
  ConstructorMap.get_constructor v.construct ptr
