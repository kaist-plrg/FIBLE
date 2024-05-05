open StdlibExt
open Notation

type t = {
  num : Int32.t;
  shift : Int32.t;
  mask : Int32.t;
  patexp : OperandExpression.t;
}

let decode (xml : Xml.xml) (sleightInit : SleighInit.t) : (t, String.t) Result.t
    =
  let* num = XmlExt.attrib_int xml "i" in
  let* shift = XmlExt.attrib_int xml "shift" in
  let* mask = XmlExt.attrib_int xml "mask" in
  let* child = XmlExt.single_child xml in
  let* patexp = OperandExpression.decode child sleightInit in
  { num; shift; mask; patexp } |> Result.ok

let pp (fmt : Format.formatter) (t : t) : unit =
  Format.fprintf fmt "{mask: %lx, shift: %ld, num: %ld, patexp: %a}" t.mask
    t.shift t.num OperandExpression.pp t.patexp

let apply (v : t)
    (resolver : OperandExpression.t -> (PatternExpression.t, String.t) Result.t)
    (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (ParserWalker.t, String.t) Result.t =
  let* pe = resolver v.patexp in
  let* ov = PatternExpression.get_value pe walker pinfo in
  let ov = Int32.shift_left (Int64.to_int32 ov) (Int32.to_int v.shift) in
  ParserWalker.setContextWord walker v.num ov v.mask
