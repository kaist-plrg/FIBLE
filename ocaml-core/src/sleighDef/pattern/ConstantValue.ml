type t = Int64.t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  let* _ = XmlExt.check_tag xml "intb" in
  XmlExt.attrib_intb xml "val"

let of_int64 (v : Int64.t) : t = v
let pp (fmt : Format.formatter) (v : t) : unit = Format.fprintf fmt "%Lx" v

let get_value (v : t) (walker : ParserWalker.t) : (Int64.t, String.t) Result.t =
  v |> Result.ok
