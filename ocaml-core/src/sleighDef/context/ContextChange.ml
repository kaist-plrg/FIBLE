open StdlibExt
open Notation

type t = Commit of ContextCommit.t | Op of ContextOp.t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  match XmlExt.tag xml with
  | "context_op" ->
      ContextOp.decode xml sleighInit |> Result.map (fun x -> Op x)
  | "commit" ->
      ContextCommit.decode xml sleighInit |> Result.map (fun x -> Commit x)
  | _ -> "Unknown context type: " ^ XmlExt.tag xml |> Result.error
