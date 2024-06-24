type t = {
  sectionId : Int32.t Option.t;
  delaySlot : Int32.t;
  numlabels : Int32.t;
  resultTpl : HandleTpl.t Option.t;
  opTpls : OpTpl.t List.t;
}

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  let sectionId = XmlExt.attrib_int xml "section" |> Result.to_option in
  let delaySlot = XmlExt.attrib_int_value xml "delay" 0l in
  let numlabels = XmlExt.attrib_int_value xml "labels" 0l in
  let childs = XmlExt.children xml in
  let* hd, ops =
    match childs with
    | hd :: ops -> (hd, ops) |> Result.ok
    | _ -> "Not matched child" |> Result.error
  in
  let* resultTpl =
    match XmlExt.tag hd with
    | "null" -> None |> Result.ok
    | _ -> HandleTpl.decode hd sleighInit |> Result.map Option.some
  in
  let* opTpls =
    List.map (fun xml -> OpTpl.decode xml sleighInit) ops |> Result.join_list
  in
  { sectionId; delaySlot; numlabels; resultTpl; opTpls } |> Result.ok

let get_sectionId (v : t) : Int32.t Option.t = v.sectionId
