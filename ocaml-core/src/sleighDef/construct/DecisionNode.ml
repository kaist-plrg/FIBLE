open StdlibExt
open Notation

type node_t = TypeDef.decision_node_t
type 'constructor_t poly_t = 'constructor_t TypeDef.decision_poly_t
type 'operand_t middle_t = 'operand_t TypeDef.decision_middle_t
type t = TypeDef.decision_unmapped
type ptr_t = TypeDef.decision_ptr_t

let rec decode (xml : Xml.xml) (table_id : Int32.t) : (ptr_t, String.t) Result.t
    =
  let* num = XmlExt.attrib_int xml "number" in
  let* contextdecision = XmlExt.attrib_bool xml "context" in
  let* startbit = XmlExt.attrib_int xml "start" in
  let* bitsize = XmlExt.attrib_int xml "size" in
  let children = XmlExt.children xml in
  let tags = List.map XmlExt.tag children in
  let* r =
    if List.for_all (String.equal "decision") tags then
      List.map (Fun.flip decode table_id) children
      |> ResultExt.join_list |> Result.map Either.right
    else if (List.for_all (String.equal "pair")) tags then
      List.map
        (fun xml ->
          let* constid = XmlExt.attrib_int xml "id" in
          let* c = XmlExt.single_child xml in
          let* pattern = DisjointPattern.decode c in
          Result.ok (pattern, ConstructorPtr.make constid table_id))
        children
      |> ResultExt.join_list |> Result.map Either.left
    else Error "Invalid children"
  in
  let node = ({ num; contextdecision; startbit; bitsize } : node_t) in
  match r with
  | Left l -> (Leaf (node, l) : ptr_t) |> Result.ok
  | Right r -> (Node (node, r) : ptr_t) |> Result.ok

let pp_node (fmt : Format.formatter) (node : node_t) : unit =
  Format.fprintf fmt "{num=%ld, contextdecision=%b, startbit=%ld, bitsize=%ld}"
    node.num node.contextdecision node.startbit node.bitsize

let rec resolve (v : 'a poly_t) (walker : ParserWalker.t) :
    ('a, String.t) Result.t =
  match v with
  | Leaf (_, patterns) -> (
      let m =
        List.filter_map
          (fun (p, c) ->
            if DisjointPattern.match_pattern p walker then Some (p, c) else None)
          patterns
      in
      match m with
      | (m, ptr) :: _ ->
          [%log debug "Matched pattern %a" DisjointPattern.pp m];
          ptr |> Result.ok
      | _ -> "No matching pattern" |> Result.error)
  | Node (node, children) ->
      [%log debug "Resolving node %a" pp_node node];
      let* index =
        if node.contextdecision then
          ParserWalker.getContextBits walker node.startbit node.bitsize
        else ParserWalker.getInstructionBits walker node.startbit node.bitsize
      in
      [%log debug "Index: %ld" index];
      let index = Int32.to_int index in
      List.nth_opt children index
      |> Option.to_result
           ~none:
             (Format.sprintf "Index %d out of bounds, %d" index
                (List.length children))
      |> Fun.flip Result.bind (fun c -> resolve c walker)
