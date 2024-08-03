open Common
module VarNode = NumericVarNode
module Assignable = AssignableF.Make (VarNode)
module ISLoadStore = ISLoadStore.Make (VarNode)
module ILoadStore = ILoadStore.Make (VarNode)
module IAssignment = IAssignment.Make (VarNode) (Assignable)
module INop = INop
module ISpecial = ISpecial

module Inst_ =
  InstF.Make5 (ILoadStore) (ISLoadStore) (IAssignment) (INop) (ISpecial)

module Inst = struct
  include Inst_
  include InstFullF.Make (Inst_)
end

module CallTarget = CallTargetF.Make (VarNode) (Unit)
module JCall = JCallF.Make (CallTarget) (StackSpaceAnnot)
module JTailCall = JTailCallF.Make (CallTarget) (StackSpaceAnnot)
module JRet = JRetF.Make (Unit)
module JIntra = JIntraF.Make (VarNode)
module Jmp = JmpFullF.MakeFromJmps (JIntra) (JCall) (JTailCall) (JRet)
module Block = BlockF.Make (Inst) (Jmp)
module Func = FuncF.Make (Jmp) (Block) (StackBoundaryAnnot)

module Prog = struct
  type t = {
    sp_num : Int32.t;
    fp_num : Int32.t;
    funcs : Func.t List.t;
    rom : DMem.t; [@opaque]
    rspec : Int32.t Int32Map.t; [@opaque]
    externs : String.t Byte8Map.t; [@opaque]
    objects : (Int64.t * String.t) List.t; [@opaque]
  }
  [@@deriving sexp, show, fields]

  let get_func_opt (p : t) (loc : Loc.t) : Func.t option =
    List.find_opt (fun (f : Func.t) -> Loc.compare f.entry loc = 0) p.funcs
end
