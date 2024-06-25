open Common
module VarNode = NumericVarNode
module Assignable = AssignableF.Make (VarNode)
module ILoadStore = ILoadStore.Make (VarNode)
module IAssignment = IAssignment.Make (VarNode) (Assignable)
module INop = INop
module Inst_ = InstF.Make3 (ILoadStore) (IAssignment) (INop)

module Inst = struct
  include Inst_
  include InstFullF.Make (Inst_)
end

module CallTarget = CallTargetF.Make (VarNode) (Unit)
module JCall = JCallF.Make (CallTarget) (Unit)
module JTailCall = JTailCallF.Make (CallTarget) (Unit)
module JRet = JRetF.Make (VarNode)
module JIntra = JIntraF.Make (VarNode)

module Jmp_ = struct
  type t =
    | JI of JIntra.t
    | JC of JCall.t
    | JT of JTailCall.t
    | JR of JRet.t
    | JswitchStop of VarNode.t
  [@@deriving sexp]

  let pp fmt (a : t) =
    match a with
    | JI i -> JIntra.pp fmt i
    | JC c -> JCall.pp fmt c
    | JT t -> JTailCall.pp fmt t
    | JR r -> JRet.pp fmt r
    | JswitchStop vn -> Format.fprintf fmt "switch stop %a;" VarNode.pp vn

  let succ jmp =
    match jmp with
    | JI i -> JIntra.succ i
    | JC c -> JCall.succ c
    | JT t -> JTailCall.succ t
    | JR r -> JRet.succ r
    | JswitchStop _ -> []

  let is_ret jmp =
    match jmp with
    | JI i -> JIntra.is_ret i
    | JC c -> JCall.is_ret c
    | JT t -> JTailCall.is_ret t
    | JR r -> JRet.is_ret r
    | JswitchStop _ -> false

  let resolve_calltarget_opt (j : t) : Loc.t option =
    match j with
    | JI i -> JIntra.resolve_calltarget_opt i
    | JC c -> JCall.resolve_calltarget_opt c
    | JT t -> JTailCall.resolve_calltarget_opt t
    | JR r -> JRet.resolve_calltarget_opt r
    | JswitchStop _ -> None
end

module Jmp = struct
  include Jmp_
  include JmpFullF.Make (Jmp_)
end

module Block = BlockF.Make (Inst) (Jmp)

module Func = struct
  include FuncF.Make (Jmp) (Block) (Unit)

  let find_switchstop_opt (f : t) : Block.t option =
    List.find_opt
      (fun (b : Block.t) ->
        match b.jmp.jmp with JswitchStop _ -> true | _ -> false)
      f.blocks
end

module Prog = struct
  type t = {
    funcs : Func.t List.t;
    rom : DMem.t; [@opaque]
    rspec : Int32.t Int32Map.t; [@opaque]
    externs : String.t Byte8Map.t; [@opaque]
  }
  [@@deriving sexp, show, fields]

  let get_func_opt (p : t) (loc : Loc.t) : Func.t option =
    List.find_opt (fun (f : Func.t) -> Loc.compare f.entry loc = 0) p.funcs
end
