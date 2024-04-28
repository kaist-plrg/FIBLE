type t = Unit.t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  match XmlExt.tag xml with
  | "tokenfield" | "contextfield" | "intb" | "start_exp" | "end_exp" ->
      () |> Result.ok
  | _ -> Format.sprintf "Not matched tag %s" (XmlExt.tag xml) |> Result.error

let check_tag (s : String.t) : Bool.t =
  match s with
  | "tokenfield" | "contextfield" | "intb" | "start_exp" | "end_exp" -> true
  | _ -> false

let of_start (() : Unit.t) : t = ()
let of_end (() : Unit.t) : t = ()
let of_start2 (() : Unit.t) : t = ()
let of_next2 (() : Unit.t) : t = ()

let pp (fmt : Format.formatter) (v : t) : Unit.t =
  Format.fprintf fmt "patternvalue"
