open Common
module JIntra = JIntraF.Make (VarNode)
include JmpFullF.MakeFromJmps (JIntra) (JCall) (JTailCall) (JRet)

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
