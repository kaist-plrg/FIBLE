open StdlibExt
open Notation

type t = { context : ContextPattern.t; instr : InstructionPattern.t }

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  let children = Xml.children xml in
  match children with
  | [ context; instr ] ->
      let* context = ContextPattern.decode context in
      let* instr = InstructionPattern.decode instr in
      { context; instr } |> Result.ok
  | _ -> Error "Expected two children"
