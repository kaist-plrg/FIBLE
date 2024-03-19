open Common

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

  let resolve_calltarget_opt (jmp : t) : Loc.t option =
    match jmp with
    | JI v -> JIntra.resolve_calltarget_opt v
    | JC v -> JCall.resolve_calltarget_opt v
    | JT v -> JTailCall.resolve_calltarget_opt v
    | JR v -> JRet.resolve_calltarget_opt v
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
