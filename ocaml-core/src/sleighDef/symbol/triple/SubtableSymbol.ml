type ('operand_t, 'constructor_t) poly_t =
  ('operand_t, 'constructor_t) TypeDef.subtable_poly_t

type 'operand_t middle_t = 'operand_t TypeDef.subtable_middle_t
type t = TypeDef.subtable_unmapped
type ptr_t = TypeDef.subtable_ptr_t

let partition_list (l : 'a List.t) (x : Int.t) : 'a List.t * 'a List.t =
  let a, b, _ =
    List.fold_left
      (fun (a, b, x) e ->
        match x with 0 -> (a, e :: b, 0) | x -> (e :: a, b, x - 1))
      ([], [], x) l
  in
  (a |> List.rev, b |> List.rev)

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (ptr_t, String.t) Result.t =
  let* numct = XmlExt.attrib_int xml "numct" in
  let consts, rest =
    partition_list (Xml.children xml) (numct |> Int32.to_int)
  in
  let* construct =
    List.map (fun xml -> Constructor.decode xml sleighInit) consts
    |> Result.join_list
  in
  let constructMap = construct |> Fun.flip ConstructorMap.of_list header.id in
  let* decisiontree =
    match rest with
    | [ xml ] -> DecisionNode.decode xml header.id
    | _ -> Result.error "Expected exactly one decision node"
  in
  ({
     name = header.name;
     id = header.id;
     scopeid = header.scopeid;
     construct = constructMap;
     decisiontree;
   }
    : ptr_t)
  |> Result.ok

let lift_middle (construct : 'operand_t TypeDef.constructor_map_poly_t)
    (s : DecisionNode.ptr_t) :
    ('operand_t DecisionNode.middle_t, String.t) Result.t =
  let rec lift_dec (d : DecisionNode.ptr_t) :
      ('a DecisionNode.middle_t, String.t) Result.t =
    match d with
    | Leaf (n, f) ->
        let* nf =
          List.map
            (fun (p, ptr) ->
              let* nptr = ConstructorMap.get_constructor construct ptr in
              (p, nptr) |> Result.ok)
            f
          |> Result.join_list
        in

        Ok (TypeDef.Leaf (n, nf))
    | Node (n, c) ->
        let* nc = List.map lift_dec c |> Result.join_list in
        Ok (TypeDef.Node (n, nc))
  in
  lift_dec s

let get_name (symbol : ('a, 'b) poly_t) : String.t = symbol.name
let get_id (symbol : ('a, 'b) poly_t) : Int32.t = symbol.id
let get_scopeid (symbol : ('a, 'b) poly_t) : Int32.t = symbol.scopeid

let get_constructor (symbol : ('a, 'b) poly_t) (ptr : ConstructorPtr.t) :
    ('a Constructor.poly_t, String.t) Result.t =
  ConstructorMap.get_constructor symbol.construct ptr
