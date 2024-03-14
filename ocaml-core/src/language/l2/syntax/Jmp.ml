open Common

module Inner = struct
  type t = JI of JIntra.t | JC of JCall.t | JT of JTailCall.t | JR of JRet.t

  let pp fmt (a : t) =
    match a with
    | JI j -> JIntra.pp fmt j
    | JC j -> JCall.pp fmt j
    | JT j -> JTailCall.pp fmt j
    | JR j -> JRet.pp fmt j

  let succ jmp =
    match jmp with
    | JI j -> JIntra.succ j
    | JC j -> JCall.succ j
    | JT j -> JTailCall.succ j
    | JR j -> JRet.succ j

  let is_ret jmp =
    match jmp with
    | JI j -> JIntra.is_ret j
    | JC j -> JCall.is_ret j
    | JT j -> JTailCall.is_ret j
    | JR j -> JRet.is_ret j

  let get_call_target (j : t) : Loc.t option =
    match j with
    | JI j -> JIntra.get_call_target j
    | JC j -> JCall.get_call_target j
    | JT j -> JTailCall.get_call_target j
    | JR j -> JRet.get_call_target j
end

include Inner
include JmpFullF.Make (Inner)
