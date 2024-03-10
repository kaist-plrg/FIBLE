open Basic
open Basic_collection
open Common_language

module Inner = struct
  type t =
    | JI of JIntra.t
    | JC of JCall.t
    | JT of JTailCall.t
    | JR of JRet.t
    | JswitchStop of VarNode.t

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

  let get_call_target (j : t) : Loc.t option =
    match j with
    | JI i -> JIntra.get_call_target i
    | JC c -> JCall.get_call_target c
    | JT t -> JTailCall.get_call_target t
    | JR r -> JRet.get_call_target r
    | JswitchStop _ -> None
end

include Inner
include JmpFullF.Make (Inner)
