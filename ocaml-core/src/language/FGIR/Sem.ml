open Syn
module Cont = Common.ContF.Make (Inst) (Jmp) (Block) (Func) (Prog)
module Value = Common.NumericValue
module StoreAction = Common.StoreActionF.Make (Value)
module TimeStamp = Common.UnitTimeStamp
module Cursor = Common.CursorF.Make (TimeStamp)
module RegFile = Common.RegFileF.Make (Value)
module Store = Common.LowStore.Make (VarNode) (Assignable)

module SCallTarget =
  Common.SCallTargetF.Make (VarNode) (CallTarget) (Value) (Store)
    (struct
      type t = Unit.t

      let eval (s : Store.t) (c : CallTarget.Attr.t) : (t, String.t) Result.t =
        () |> Result.ok
    end)

module SCall =
  Common.SCallF.Make (VarNode) (CallTarget) (JCall) (Value) (Store)
    (SCallTarget)
    (struct
      type t = Unit.t

      let eval (s : Store.t) (c : JCall.Attr.t) : (t, String.t) Result.t =
        () |> Result.ok
    end)

module STailCall =
  Common.STailCallF.Make (VarNode) (CallTarget) (JTailCall) (Value) (Store)
    (SCallTarget)
    (struct
      type t = Unit.t

      let eval (s : Store.t) (c : JTailCall.Attr.t) : (t, String.t) Result.t =
        () |> Result.ok
    end)

module SRet =
  Common.SRetF.Make (VarNode) (JRet) (Value) (Store)
    (struct
      type t = Value.t

      let eval (s : Store.t) (c : JRet.Attr.t) : (t, String.t) Result.t =
        Store.eval_vn s c
    end)

module Action =
  Common.HighActionF.Make (Value) (StoreAction) (SCall) (STailCall) (SRet)

module Stack = struct
  open Common

  type elem_t = Cursor.t * Loc.t
  type t = elem_t List.t

  let pp (fmt : Format.formatter) (t : t) : unit =
    let pp_elem_t (fmt : Format.formatter) (t : elem_t) : unit =
      let l1, l2 = t in
      Format.fprintf fmt "(%a, %a)" Cursor.pp l1 Loc.pp l2
    in
    Format.fprintf fmt "[%a]" (Format.pp_print_list pp_elem_t) t

  let get_cursor (v : elem_t) : Cursor.t = fst v
  let get_fallthrough (v : elem_t) : Loc.t = snd v
end

module State =
  Common.HighStateF.Make (Func) (Prog) (VarNode) (CallTarget) (JCall)
    (JTailCall)
    (JRet)
    (JIntra)
    (TimeStamp)
    (Value)
    (Store)
    (SCallTarget)
    (SCall)
    (STailCall)
    (SRet)
    (Action)
    (Cont)
    (Cursor)
    (Stack)
    (World.Environment)
