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

let pp (fmt : Format.formatter) (x : t) : unit =
  match x with Commit x -> ContextCommit.pp fmt x | Op x -> ContextOp.pp fmt x

let apply (v : t) (walker : ParserWalker.t) :
    (ParserWalker.t, String.t) Result.t =
  match v with
  | Commit v -> ContextCommit.apply v walker
  | Op v -> ContextOp.apply v walker
