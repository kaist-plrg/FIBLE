open StdlibExt
open Notation

type t = {
  opc : String.t;
  out : VarNodeTpl.t Option.t;
  ins : VarNodeTpl.t List.t;
}

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  let* opc = XmlExt.attrib xml "code" in
  let childs = XmlExt.children xml in
  let* hd, ins =
    match childs with
    | hd :: ins -> (hd, ins) |> Result.ok
    | _ -> "not matched children" |> Result.error
  in
  let* out =
    match XmlExt.tag hd with
    | "null" -> None |> Result.ok
    | _ -> VarNodeTpl.decode hd sleighInit |> Result.map Option.some
  in
  let* ins =
    ins
    |> List.map (fun xml -> VarNodeTpl.decode xml sleighInit)
    |> ResultExt.join_list
  in
  { opc; out; ins } |> Result.ok
