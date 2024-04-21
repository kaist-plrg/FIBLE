open StdlibExt
open Notation

type t = {
  offset : Int32.t;
  nonzerosize : Int32.t;
  maskvalues : (Int32.t * Int32.t) List.t;
}

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  let* () = XmlExt.check_tag xml "pat_block" in
  let* offset = XmlExt.attrib_int xml "offset" in
  let* nonzerosize = XmlExt.attrib_int xml "nonzero" in
  let* maskvalues =
    XmlExt.children xml
    |> List.map (fun xml ->
           let* () = XmlExt.check_tag xml "mask_word" in
           let* mask = XmlExt.attrib_int xml "mask" in
           let* value = XmlExt.attrib_int xml "val" in
           (mask, value) |> Result.ok)
    |> ResultExt.join_list
  in
  { offset; nonzerosize; maskvalues } |> Result.ok
