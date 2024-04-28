open StdlibExt
open Notation

module Inner = struct
  type t = Pat of PatternValue.t | Oper of OperandValue.t

  let decode (xml : Xml.xml) (sleighInit : SleighInit.t) :
      (t, String.t) Result.t =
    let tag = XmlExt.tag xml in
    if String.equal tag "operand_exp" then
      OperandValue.decode xml sleighInit |> Result.map (fun v -> Oper v)
    else PatternValue.decode xml sleighInit |> Result.map (fun v -> Pat v)

  let check_tag (s : String.t) : Bool.t =
    match s with "operand_exp" -> true | _ -> PatternValue.check_tag s

  let pp (fmt : Format.formatter) (v : t) : Unit.t =
    match v with
    | Pat e -> PatternValue.pp fmt e
    | Oper v -> OperandValue.pp fmt v

  let of_start (() : Unit.t) : t = Pat (PatternValue.of_start ())
  let of_end (() : Unit.t) : t = Pat (PatternValue.of_end ())
  let of_start2 (() : Unit.t) : t = Pat (PatternValue.of_start2 ())
  let of_next2 (() : Unit.t) : t = Pat (PatternValue.of_next2 ())
end

include PatternExpressionF.Make (Inner)
