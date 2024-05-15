open StdlibExt
open Notation

type t = { space : ConstTpl.t; offset : ConstTpl.t; size : ConstTpl.t }

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  let child = XmlExt.children xml in
  let* spacexml, offsetxml, sizexml =
    match child with
    | [ spacexml; offsetxml; sizexml ] -> Ok (spacexml, offsetxml, sizexml)
    | _ -> Error "Expecting exactly 3 children"
  in
  let* space = ConstTpl.decode spacexml sleighInit in
  let* offset = ConstTpl.decode offsetxml sleighInit in
  let* size = ConstTpl.decode sizexml sleighInit in
  { space; offset; size } |> Result.ok

let get_offset (v : t) : ConstTpl.t = v.offset

let is_dynamic (v : t) (operands : FixedHandle.t List.t)
    (walker : ParserWalker.t) : (Bool.t, String.t) Result.t =
  match v.offset with
  | Handle v ->
      let* res =
        List.nth_opt operands (Int32.to_int v.handleInd)
        |> Option.to_result ~none:"Operand index out of bounds"
      in
      Option.is_some res.offset_space |> Result.ok
  | _ -> false |> Result.ok

let generateLocation (v : t) (opers : FixedHandle.t List.t)
    (pinfo : PatternInfo.t) : (VarNode.t, String.t) Result.t =
  let* space = ConstTpl.fixSpace v.space opers in
  let* size = ConstTpl.fix v.size opers pinfo |> Result.map Int64.to_int32 in
  let* offset =
    if AddrSpace.is_const_space space then ConstTpl.fix v.offset opers pinfo
    else if AddrSpace.is_unique_space space then
      ConstTpl.fix v.offset opers pinfo
      |> Result.map (Int64.logor pinfo.uoffset)
    else ConstTpl.fix v.offset opers pinfo
  in
  VarNode.make (AddrSpace.get_index space |> Int32.of_int) size offset
  |> Result.ok

let generatePointer (v : t) (opers : FixedHandle.t List.t)
    (pinfo : PatternInfo.t) : (VarNode.t * AddrSpace.t, String.t) Result.t =
  let* hind =
    ConstTpl.try_handle v.offset
    |> Option.map fst
    |> Option.to_result ~none:"No handle"
  in
  let* op =
    List.nth_opt opers (Int32.to_int hind)
    |> Option.to_result ~none:[%logstr "Operand index out of bounds"]
  in
  let* space = op.offset_space |> Option.to_result ~none:[%logstr "No space"] in
  let size = op.offset_size in
  let offset =
    if AddrSpace.is_const_space space then op.offset_offset
    else if AddrSpace.is_unique_space space then
      Int64.logor op.offset_offset pinfo.uoffset
    else op.offset_offset
  in
  ( VarNode.make (AddrSpace.get_index space |> Int32.of_int) size offset,
    op.space )
  |> Result.ok
