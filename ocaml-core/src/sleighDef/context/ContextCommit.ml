type t = { symId : TuplePtr.t; num : Int32.t; mask : Int32.t; flow : Bool.t }

let decode (xml : Xml.xml) (sleightInit : SleighInit.t) : (t, String.t) Result.t
    =
  let* symId = XmlExt.attrib_int xml "id" |> Result.map TuplePtr.of_int32 in
  let* num = XmlExt.attrib_int xml "num" in
  let* mask = XmlExt.attrib_int xml "mask" in
  let* flow = XmlExt.attrib_bool xml "flow" in
  { symId; num; mask; flow } |> Result.ok

let pp (fmt : Format.formatter) (sleight : t) : unit =
  Format.fprintf fmt "symId: %ld, num: %ld, mask: %ld, flow: %b"
    (TuplePtr.get_id sleight.symId)
    sleight.num sleight.mask sleight.flow

let apply (v : t) (walker : ParserWalker.t) :
    (ParserWalker.t, String.t) Result.t =
  [%log info "WARN: not implemented"];
  walker |> Result.ok
