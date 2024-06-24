type t =
  | Token of TokenField.t
  | Context of ContextField.t
  | Constant of ConstantValue.t
  | Start of StartInstructionValue.t
  | End of EndInstructionValue.t
  | Next2 of Next2InstructionValue.t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  match XmlExt.tag xml with
  | "tokenfield" ->
      TokenField.decode xml sleighInit |> Result.map (fun x -> Token x)
  | "contextfield" ->
      ContextField.decode xml sleighInit |> Result.map (fun x -> Context x)
  | "intb" ->
      ConstantValue.decode xml sleighInit |> Result.map (fun x -> Constant x)
  | "start_exp" ->
      StartInstructionValue.decode xml sleighInit
      |> Result.map (fun x -> Start x)
  | "end_exp" ->
      EndInstructionValue.decode xml sleighInit |> Result.map (fun x -> End x)
  | _ -> Format.sprintf "Not matched tag %s" (XmlExt.tag xml) |> Result.error

let check_tag (s : String.t) : Bool.t =
  match s with
  | "tokenfield" | "contextfield" | "intb" | "start_exp" | "end_exp" -> true
  | _ -> false

let of_start (() : Unit.t) : t = Start ()
let of_end (() : Unit.t) : t = End ()
let of_next2 (() : Unit.t) : t = Next2 ()

let pp (fmt : Format.formatter) (v : t) : Unit.t =
  match v with
  | Token x -> TokenField.pp fmt x
  | Context x -> ContextField.pp fmt x
  | Constant x -> ConstantValue.pp fmt x
  | Start x -> StartInstructionValue.pp fmt x
  | End x -> EndInstructionValue.pp fmt x
  | Next2 x -> Next2InstructionValue.pp fmt x

let get_value (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (Int64.t, String.t) Result.t =
  match v with
  | Token x -> TokenField.get_value x walker
  | Context x -> ContextField.get_value x walker
  | Constant x -> ConstantValue.get_value x walker
  | Start x -> StartInstructionValue.get_value x walker pinfo
  | End x -> EndInstructionValue.get_value x walker pinfo
  | Next2 x -> Next2InstructionValue.get_value x walker pinfo
