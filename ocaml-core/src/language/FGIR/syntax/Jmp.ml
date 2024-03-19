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

let from_partial (j : FGIR_partial.Jmp.t_full) : t_full =
  let njmp =
    match j.jmp with
    | FGIR_partial.Jmp.JI v -> JI v
    | FGIR_partial.Jmp.JC v -> JC v
    | FGIR_partial.Jmp.JT v -> JT v
    | FGIR_partial.Jmp.JR v -> JR v
    | FGIR_partial.Jmp.JswitchStop _ -> JI Junimplemented
  in
  { jmp = njmp; loc = j.loc; mnem = j.mnem }
