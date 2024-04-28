open StdlibExt
open Notation

type t = {
  num : Int32.t;
  shift : Int32.t;
  mask : Int32.t;
  patexp : ContextExpression.t;
}

let decode (xml : Xml.xml) (sleightInit : SleighInit.t) : (t, String.t) Result.t
    =
  let* num = XmlExt.attrib_int xml "i" in
  let* shift = XmlExt.attrib_int xml "shift" in
  let* mask = XmlExt.attrib_int xml "mask" in
  let* child = XmlExt.single_child xml in
  let* patexp = ContextExpression.decode child sleightInit in
  { num; shift; mask; patexp } |> Result.ok

let pp (fmt : Format.formatter) (t : t) : unit =
  Format.fprintf fmt "{mask: %lx, shift: %ld, num: %ld, patexp: %a}" t.mask
    t.shift t.num ContextExpression.pp t.patexp

let apply (v : t) (walker : ParserWalker.t) :
    (ParserWalker.t, String.t) Result.t =
  walker |> Result.ok
