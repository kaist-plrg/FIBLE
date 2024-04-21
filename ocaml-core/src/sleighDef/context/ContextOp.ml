open StdlibExt
open Notation

type t = {
  num : Int32.t;
  shift : Int32.t;
  mask : Int32.t;
  patexp : PatternValue.t;
}

let decode (xml : Xml.xml) (sleightInit : SleighInit.t) : (t, String.t) Result.t
    =
  let* num = XmlExt.attrib_int xml "i" in
  let* shift = XmlExt.attrib_int xml "shift" in
  let* mask = XmlExt.attrib_int xml "mask" in
  let* child = XmlExt.single_child xml in
  let* patexp =
    PatternExpression.decode child sleightInit
    |> Fun.flip Result.bind (fun x ->
           PatternExpression.to_value x
           |> Option.to_result ~none:"PatternExpression.decode failed")
  in
  { num; shift; mask; patexp } |> Result.ok
