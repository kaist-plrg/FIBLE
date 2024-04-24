open StdlibExt
open Notation

type 'operand_t poly_t = 'operand_t TypeDef.constructor_poly_t
type t = TypeDef.constructor_t
type ptr_t = TypeDef.constructor_ptr_t

let split (childs : Xml.xml List.t) :
    Xml.xml List.t * Xml.xml List.t * Xml.xml List.t * Xml.xml List.t =
  let aux (child : Xml.xml) (opers, prints, contexts, tmpls) =
    match Xml.tag child with
    | "oper" -> (child :: opers, prints, contexts, tmpls)
    | "print" | "opprint" -> (opers, child :: prints, contexts, tmpls)
    | "context_op" | "commit" -> (opers, prints, child :: contexts, tmpls)
    | "construct_tpl" -> (opers, prints, contexts, child :: tmpls)
    | _ -> (opers, prints, contexts, tmpls)
  in
  List.fold_right aux childs ([], [], [], [])

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) :
    (ptr_t, String.t) Result.t =
  let* parentId =
    XmlExt.attrib_int xml "parent" |> Result.map SubtablePtr.of_int32
  in
  let* firstWhitespace = XmlExt.attrib_int xml "first" in
  let* minimumlength = XmlExt.attrib_int xml "length" in
  let childs = XmlExt.children xml in
  let opers, prints, contexts, tmpls = split childs in
  let* operandIds =
    List.map
      (fun xml -> XmlExt.attrib_int xml "id" |> Result.map OperandPtr.of_int32)
      opers
    |> ResultExt.join_list
  in
  let* printpieces =
    List.map
      (fun xml ->
        match XmlExt.tag xml with
        | "print" ->
            let* str = XmlExt.attrib xml "piece" in
            TypeDef.Str str |> Result.ok
        | "opprint" ->
            let* id = XmlExt.attrib_int xml "id" in
            TypeDef.OperInd id |> Result.ok
        | _ -> Error "Invalid tag")
      prints
    |> ResultExt.join_list
  in
  let* context =
    List.map (fun xml -> ContextChange.decode xml sleighInit) contexts
    |> ResultExt.join_list
  in
  let* tmpls =
    List.map (fun xml -> ConstructTpl.decode xml sleighInit) tmpls
    |> ResultExt.join_list
  in
  let* tmpl =
    List.find_opt
      (fun tmpl -> Option.is_none (ConstructTpl.get_sectionId tmpl))
      tmpls
    |> Option.to_result ~none:"No main tmpl"
  in
  let namedtmpl =
    List.fold_left
      (fun acc tmpl ->
        match ConstructTpl.get_sectionId tmpl with
        | Some id -> Int32Map.add id tmpl acc
        | None -> acc)
      Int32Map.empty tmpls
  in
  let flowthruIndex =
    match printpieces with [ OperInd i ] -> Some i | _ -> None
  in
  {
    TypeDef.parentId;
    minimumlength;
    firstWhitespace;
    flowthruIndex;
    operandIds;
    printpieces;
    context;
    tmpl;
    namedtmpl;
  }
  |> Result.ok

let pp_printpiece (fmt : Format.formatter) (v : 'a poly_t) : Unit.t =
  let pp_printpiece fmt = function
    | TypeDef.Str s -> Format.fprintf fmt "%s" s
    | TypeDef.OperInd i -> Format.fprintf fmt "@<%ld>" i
  in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
    pp_printpiece fmt v.printpieces
