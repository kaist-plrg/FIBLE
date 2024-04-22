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

let pp (fmt : Format.formatter) (pattern : t) : unit =
  match pattern with
  | Context v -> ContextPattern.pp fmt v
  | Instruction v -> InstructionPattern.pp fmt v
  | Combine v -> CombinePattern.pp fmt v

let match_pattern (pattern : t) (walker : ParserWalker.t) : Bool.t = true
(*
  match pattern with
  | Context { pattern } -> PatternBlock.isContextMatch pattern walker
  | Instruction { pattern } -> PatternBlock.isInstructionMatch pattern walker
  | Combine { context = { pattern = cpattern }; instr = { pattern = ipattern } }
    ->
      PatternBlock.isContextMatch cpattern walker
      && PatternBlock.isInstructionMatch ipattern walker
*)
