type t = Unit.t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  () |> Result.ok

let pp (fmt : Format.formatter) (t : t) : unit = Format.fprintf fmt "[end]"

let get_value (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (Int64.t, String.t) Result.t =
  pinfo.naddr |> Result.ok
