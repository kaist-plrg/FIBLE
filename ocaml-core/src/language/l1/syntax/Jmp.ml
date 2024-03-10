open Basic
open Basic_collection
open Common_language

module Inner = struct
  type t = JI of JIntra.t | JC of JCall.t | JT of JTailCall.t | JR of JRet.t

  let pp fmt (jmp : t) =
    match jmp with
    | JI v -> JIntra.pp fmt v
    | JC v -> JCall.pp fmt v
    | JT v -> JTailCall.pp fmt v
    | JR v -> JRet.pp fmt v

  let succ jmp =
    match jmp with
    | JI v -> JIntra.succ v
    | JC v -> JCall.succ v
    | JT v -> JTailCall.succ v
    | JR v -> JRet.succ v

  let is_ret jmp =
    match jmp with
    | JI v -> JIntra.is_ret v
    | JC v -> JCall.is_ret v
    | JT v -> JTailCall.is_ret v
    | JR v -> JRet.is_ret v

  let get_call_target (jmp : t) : Loc.t option =
    match jmp with
    | JI v -> JIntra.get_call_target v
    | JC v -> JCall.get_call_target v
    | JT v -> JTailCall.get_call_target v
    | JR v -> JRet.get_call_target v
end

include Inner
include JmpFullF.Make (Inner)

let from_partial (j : L1Partial.Jmp.t_full) : t_full =
  let njmp =
    match j.jmp with
    | L1Partial.Jmp.JI v -> JI v
    | L1Partial.Jmp.JC v -> JC v
    | L1Partial.Jmp.JT v -> JT v
    | L1Partial.Jmp.JR v -> JR v
    | L1Partial.Jmp.JswitchStop _ -> JI Junimplemented
  in
  { jmp = njmp; loc = j.loc; mnem = j.mnem }
