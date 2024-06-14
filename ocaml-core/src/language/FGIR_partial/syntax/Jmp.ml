open Common
module JIntra = JIntraF.Make (VarNode)

module Inner = struct
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

include Inner
include JmpFullF.Make (Inner)
