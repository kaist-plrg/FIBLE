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
