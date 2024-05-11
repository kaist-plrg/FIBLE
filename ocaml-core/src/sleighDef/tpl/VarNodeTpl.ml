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

let generateLocation (v : t) (opers : FixedHandle.t List.t) :
    (VarNode.t, String.t) Result.t =
  let* space = ConstTpl.fixSpace v.space opers in
  let* size = ConstTpl.fix v.size opers |> Result.map Int64.to_int32 in
  let* offset =
    if AddrSpace.is_const_space space then ConstTpl.fix v.offset opers
    else if AddrSpace.is_unique_space space then ConstTpl.fix v.offset opers
    else ConstTpl.fix v.offset opers
  in
  VarNode.make space size offset |> Result.ok
