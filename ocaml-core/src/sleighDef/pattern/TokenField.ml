open StdlibExt
open Notation

type t = {
  bigendian : Bool.t;
  signbit : Bool.t;
  bitstart : Int32.t;
  bitend : Int32.t;
  bytestart : Int32.t;
  byteend : Int32.t;
  shift : Int32.t;
}

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  let* _ = XmlExt.check_tag xml "tokenfield" in
  let* bigendian = XmlExt.attrib_bool xml "bigendian" in
  let* signbit = XmlExt.attrib_bool xml "signbit" in
  let* bitstart = XmlExt.attrib_int xml "bitstart" in
  let* bitend = XmlExt.attrib_int xml "bitend" in
  let* bytestart = XmlExt.attrib_int xml "bytestart" in
  let* byteend = XmlExt.attrib_int xml "byteend" in
  let* shift = XmlExt.attrib_int xml "shift" in
  { bigendian; signbit; bitstart; bitend; bytestart; byteend; shift }
  |> Result.ok

let pp (fmt : Format.formatter) (t : t) : unit = Format.fprintf fmt "token"

let get_value (v : t) (walker : ParserWalker.t) : (Int64.t, String.t) Result.t =
  (* TODO: add bigendian *)
  let* res =
    ParserWalker.getInstructionBytes walker v.bytestart
      (Int32.succ (Int32.sub v.byteend v.bytestart))
  in
  let res =
    Int32.shift_right_logical res (Int32.to_int v.shift) |> Int64.of_int32
  in
  [%log debug "res: %Lx" res];
  let* v =
    (if v.signbit then
       Int64Ext.sext_bit res (Int32.succ (Int32.sub v.bitend v.bitstart)) 64l
     else Int64Ext.zext_bit res (Int32.succ (Int32.sub v.bitend v.bitstart)) 64l)
    |> Result.ok
  in
  [%log debug "v: %Lx" v];
  Result.ok v
