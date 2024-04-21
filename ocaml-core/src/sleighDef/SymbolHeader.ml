open StdlibExt
open Notation

type t = { tag : SymbolTag.t; name : String.t; id : Int32.t; scopeid : Int32.t }

let str_to_tag (s : String.t) : (SymbolTag.t, String.t) Result.t =
  match s with
  | "userop_head" -> SymbolTag.TUserOp |> Result.ok
  | "epsilon_sym_head" -> SymbolTag.TEpsilon |> Result.ok
  | "value_sym_head" -> SymbolTag.TPureValue |> Result.ok
  | "valuemap_sym_head" -> SymbolTag.TValueMap |> Result.ok
  | "name_sym_head" -> SymbolTag.TName |> Result.ok
  | "varnode_sym_head" -> SymbolTag.TVarNode |> Result.ok
  | "context_sym_head" -> SymbolTag.TContext |> Result.ok
  | "varlist_sym_head" -> SymbolTag.TVarNodeList |> Result.ok
  | "operand_sym_head" -> SymbolTag.TOperand |> Result.ok
  | "start_sym_head" -> SymbolTag.TStart |> Result.ok
  | "end_sym_head" -> SymbolTag.TEnd |> Result.ok
  | "next2_sym_head" -> SymbolTag.TNext2 |> Result.ok
  | "subtable_sym_head" -> SymbolTag.TSubtable |> Result.ok
  | _ -> Error "Unknown tag"

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  let* tag = XmlExt.tag xml |> str_to_tag in
  let* name = XmlExt.attrib xml "name" in
  let* id = XmlExt.attrib_int xml "id" in
  let* scopeid = XmlExt.attrib_int xml "scope" in
  { tag; name; id; scopeid } |> Result.ok
