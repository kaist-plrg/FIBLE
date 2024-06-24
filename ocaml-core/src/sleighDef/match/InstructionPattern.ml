type t = { pattern : PatternBlock.t }

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  let* () = XmlExt.check_tag xml "instruct_pat" in
  let* child = XmlExt.single_child xml in
  let* pattern = PatternBlock.decode child in
  { pattern } |> Result.ok

let pp (fmt : Format.formatter) (t : t) : unit =
  Format.fprintf fmt "@[<v 2>Instruction Pattern:@,%a@]" PatternBlock.pp
    t.pattern
