open StdlibExt
open Notation

type node_t = {
  num : Int32.t;
  contextdecision : Bool.t;
  startbit : Int32.t;
  bitsize : Int32.t;
}

type t =
  | Leaf of node_t * (DisjointPattern.t * Int32.t) List.t
  | Node of node_t * t List.t

let rec decode (xml : Xml.xml) : (t, String.t) Result.t =
  let* num = XmlExt.attrib_int xml "number" in
  let* contextdecision = XmlExt.attrib_bool xml "context" in
  let* startbit = XmlExt.attrib_int xml "start" in
  let* bitsize = XmlExt.attrib_int xml "size" in
  let children = XmlExt.children xml in
  let tags = List.map XmlExt.tag children in
  let* r =
    if List.for_all (String.equal "decision") tags then
      List.map decode children |> ResultExt.join_list |> Result.map Either.right
    else if (List.for_all (String.equal "pair")) tags then
      List.map
        (fun xml ->
          let* constid = XmlExt.attrib_int xml "id" in
          let* c = XmlExt.single_child xml in
          let* pattern = DisjointPattern.decode c in
          Result.ok (pattern, constid))
        children
      |> ResultExt.join_list |> Result.map Either.left
    else Error "Invalid children"
  in
  let node = { num; contextdecision; startbit; bitsize } in
  match r with
  | Left l -> Leaf (node, l) |> Result.ok
  | Right r -> Node (node, r) |> Result.ok

let resolve (v : t) (walker : ParserWalker.t) : (Int32.t, String.t) Result.t =
  "not impl" |> Result.error
