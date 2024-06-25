open Common
module VarNode = NumericVarNode
module Assignable = AssignableF.Make (VarNode)
module ISLoadStore = ISLoadStore.Make (VarNode)
module ILoadStore = ILoadStore.Make (VarNode)
module IAssignment = IAssignment.Make (VarNode) (Assignable)
module INop = INop
module Inst_ = InstF.Make4 (ILoadStore) (ISLoadStore) (IAssignment) (INop)

module Inst = struct
  include Inst_
  include InstFullF.Make (Inst_)
end

module CTAnnot = Common.InputOutputAnnot.Make (VarNode)
module CallTarget = CallTargetF.Make (VarNode) (CTAnnot)
module JCall = JCallF.Make (CallTarget) (StackSpaceAnnot)
module TAnnot = Common.IORAnnot.Make (VarNode)
module JTailCall = JTailCallF.Make (CallTarget) (TAnnot)
module RAnnot = Common.ReturnValueAnnot.Make (VarNode)
module JRet = JRetF.Make (RAnnot)
module JIntra = JIntraF.Make (VarNode)
module Jmp = JmpFullF.MakeFromJmps (JIntra) (JCall) (JTailCall) (JRet)
module Block = BlockF.Make (Inst) (Jmp)
module Func = FuncF.Make (Jmp) (Block) (SBAndIOAnnot)

module Prog = struct
  type t = {
    sp_num : Int32.t;
    fp_num : Int32.t;
    funcs : Func.t List.t;
    rom : DMem.t; [@opaque]
    rspec : Int32.t Int32Map.t; [@opaque]
    externs : String.t Byte8Map.t; [@opaque]
  }
  [@@deriving sexp, show, fields]

  let get_func_opt (p : t) (loc : Loc.t) : Func.t option =
    List.find_opt (fun (f : Func.t) -> Loc.compare f.entry loc = 0) p.funcs
end
