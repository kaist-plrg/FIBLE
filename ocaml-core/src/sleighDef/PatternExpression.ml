open StdlibExt
open Notation

type t = Unit.t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  () |> Result.ok

let of_start (() : Unit.t) : t = ()
let of_end (() : Unit.t) : t = ()
let of_next2 (() : Unit.t) : t = ()
let to_value (t : t) : PatternValue.t Option.t = () |> Option.some
