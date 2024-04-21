open StdlibExt
open Notation

type t =
  | Context of ContextPattern.t
  | Instruction of InstructionPattern.t
  | Combine of CombinePattern.t

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  let tag = XmlExt.tag xml in
  match tag with
  | "context_pat" ->
      let* v = ContextPattern.decode xml in
      Context v |> Result.ok
  | "instruct_pat" ->
      let* v = InstructionPattern.decode xml in
      Instruction v |> Result.ok
  | "combine_pat" ->
      let* v = CombinePattern.decode xml in
      Combine v |> Result.ok
  | _ -> Result.Error "unknown tag"
