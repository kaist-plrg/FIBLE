type t = Unit.t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  () |> Result.ok

let pp (fmt : Format.formatter) (t : t) : unit = Format.fprintf fmt "[next2]"

let get_value (v : t) (walker : ParserWalker.t) : (Int64.t, String.t) Result.t =
  "not impl: next2 get_value" |> Result.error
