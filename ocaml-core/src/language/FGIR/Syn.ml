open Common
module VarNode = NumericVarNode
module Assignable = AssignableF.Make (VarNode)
module ILoadStore = ILoadStore.Make (VarNode)
module IAssignment = IAssignment.Make (VarNode) (Assignable)
module INop = INop
module ISpecial = ISpecial
module Inst_ = InstF.Make4 (ILoadStore) (IAssignment) (INop) (ISpecial)

module Inst = struct
  include Inst_
  include InstFullF.Make (Inst_)
end

module CallTarget = CallTargetF.Make (VarNode) (Unit)
module JCall = JCallF.Make (CallTarget) (Unit)
module JTailCall = JTailCallF.Make (CallTarget) (Unit)
module JRet = JRetF.Make (VarNode)
module JIntra = JIntraF.Make (VarNode)

module Jmp = struct
  include JmpFullF.MakeFromJmps (JIntra) (JCall) (JTailCall) (JRet)

  let from_partial (j : FGIR_partial.Syn.Jmp.t_full) : t_full =
    let njmp =
      match j.jmp with
      | JI v -> JI v
      | JC v -> JC v
      | JT v -> JT v
      | JR v -> JR v
      | JswitchStop _ -> JI Junimplemented
    in
    { jmp = njmp; loc = j.loc; mnem = j.mnem }
end

module Block = struct
  module Jmp_ = Jmp
  include BlockF.Make (Inst) (Jmp)

  let from_partial (b : FGIR_partial.Syn.Block.t) : t =
    { fLoc = b.fLoc; loc = b.loc; body = b.body; jmp = Jmp_.from_partial b.jmp }
end

module Func = struct
  module Block_ = Block
  include FuncF.Make (Jmp) (Block) (Unit)

  let from_partial (p : FGIR_partial.Syn.Func.t) : t =
    let entry = p.entry in
    let boundaries = p.boundaries in
    let blocks = List.map Block.from_partial p.blocks in
    let nameo = p.nameo in
    { nameo; entry; boundaries; blocks; attr = () }
end

module Prog = struct
  type t = {
    funcs : Func.t List.t;
    rom : DMem.t; [@opaque]
    rspec : Int32.t Int32Map.t; [@opaque]
    externs : String.t Byte8Map.t; [@opaque]
    objects : (Int64.t * String.t) List.t; [@opaque]
  }
  [@@deriving sexp, show, fields]

  let get_func_opt (p : t) (loc : Loc.t) : Func.t option =
    List.find_opt (fun (f : Func.t) -> Loc.compare f.entry loc = 0) p.funcs

  let from_partial (p : FGIR_partial.Syn.Prog.t) : t =
    let funcs = List.map Func.from_partial p.funcs in
    let rom = p.rom in
    let rspec = p.rspec in
    let externs = p.externs in
    let objects = p.objects in
    { funcs; rom; rspec; externs; objects }
end
