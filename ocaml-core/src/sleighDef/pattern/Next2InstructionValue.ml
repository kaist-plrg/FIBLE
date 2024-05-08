type t = Unit.t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  () |> Result.ok

let pp (fmt : Format.formatter) (t : t) : unit = Format.fprintf fmt "[next2]"

let get_value (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (Int64.t, String.t) Result.t =
  pinfo.n2addr |> Option.to_result ~none:"n2addr not calculated"
