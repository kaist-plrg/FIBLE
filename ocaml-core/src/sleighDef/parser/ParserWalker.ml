open StdlibExt
open Notation

type t = { const_context : ParserContext.t; point : ConstructState.t }

let getContextBytes (v : t) (startbyte : Int32.t) (bytesize : Int32.t) =
  ParserContext.getContextBytes v.const_context startbyte bytesize

let getContextBits (v : t) (startbit : Int32.t) (bitsize : Int32.t) =
  ParserContext.getContextBits v.const_context startbit bitsize

let setContextWord (v : t) (i : Int32.t) (vb : Int32.t) (mask : Int32.t) :
    (t, String.t) Result.t =
  let* const_context = ParserContext.setContextWord v.const_context i vb mask in
  { v with const_context } |> Result.ok

let getInstructionBits (v : t) (startbit : Int32.t) (bitsize : Int32.t) =
  ParserContext.getInstructionBits v.const_context startbit bitsize
    v.point.offset

let getInstructionBytes (v : t) (startbyte : Int32.t) (bytesize : Int32.t) =
  ParserContext.getInstructionBytes v.const_context startbyte bytesize
    v.point.offset

let get_offset (v : t) = v.point.offset

let replace_offset (v : t) (offset : Int32.t) =
  { v with point = { v.point with offset } }

let from_context (pc : ParserContext.t) : t =
  let point : ConstructState.t =
    { offset = 0l; ct = ConstructorPtr.make (-1l) 0l }
  in
  { const_context = pc; point }

let of_mock (s : String.t) : t =
  let const_context = ParserContext.of_mock s in
  let point : ConstructState.t =
    { offset = 0l; ct = ConstructorPtr.make (-1l) 0l }
  in
  { const_context; point }
