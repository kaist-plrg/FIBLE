open Common
module JIntra = JIntraF.Make (VarNode)
include JmpFullF.MakeFromJmps (JIntra) (JCall) (JTailCall) (JRet)
