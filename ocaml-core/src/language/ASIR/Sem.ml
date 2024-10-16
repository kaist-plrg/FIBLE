open Syn
module NonNumericValue = NonNumericValue
module Cont = Common.ContF.Make (Inst) (Jmp) (Block) (Func) (Prog)
module Pointer = Common.GlobalAndStackPointer.Make (Common.Byte8) (Common.SPVal)
module Value = Common.ValueF.Make (Pointer) (NonNumericValue)
module StoreAction = Common.StoreActionF.Make (Value)
module TimeStamp = Common.Int64TimeStamp
module Cursor = Common.CursorF.Make (TimeStamp)
module RegFile = Common.RegFileF.Make (Value)
module Frame = Common.FrameF.Make (Value)
module LocalMemory = Common.LocalMemoryF.Make (Value) (Frame)
module GlobalMemory = Common.GlobalMemoryF.Make (Value)

module Memory =
  Common.MemoryF.Make (Pointer) (Value) (TimeStamp) (Frame) (GlobalMemory)
    (LocalMemory)

module Store =
  Common.HighStoreF.Make (Prog) (Common.NumericConst) (VarNode) (Pointer)
    (Value)
    (Value)
    (StoreAction)
    (Cursor)
    (RegFile)
    (Memory)
    (Frame)

module SCallTarget =
  Common.SCallTargetF.Make (VarNode) (CallTarget) (Value) (Store)
    (struct
      type t = Unit.t [@@deriving show]

      let eval (s : Store.t) (c : CallTarget.Attr.t) : (t, String.t) Result.t =
        () |> Result.ok
    end)

module SCall =
  Common.SCallF.Make (VarNode) (CallTarget) (JCall) (Value) (Store)
    (SCallTarget)
    (struct
      type t = JCall.Attr.t [@@deriving show]

      let eval (s : Store.t) (c : JCall.Attr.t) : (t, String.t) Result.t =
        c |> Result.ok
    end)

module STailCall =
  Common.STailCallF.Make (VarNode) (CallTarget) (JTailCall) (Value) (Store)
    (SCallTarget)
    (struct
      type t = JTailCall.Attr.t [@@deriving show]

      let eval (s : Store.t) (c : JTailCall.Attr.t) : (t, String.t) Result.t =
        c |> Result.ok
    end)

module SRet =
  Common.SRetF.Make (VarNode) (JRet) (Value) (Store)
    (struct
      type t = JRet.Attr.t [@@deriving show]

      let eval (s : Store.t) (c : JRet.Attr.t) : (t, String.t) Result.t =
        c |> Result.ok
    end)

module Action =
  Common.HighActionF.Make (Value) (StoreAction) (SCall) (STailCall) (SRet)

module Stack = struct
  open Common

  type elem_t = Cursor.t * Value.t * Loc.t
  type t = elem_t List.t

  let pp (fmt : Format.formatter) (v : t) : unit =
    let pp_elem_t (fmt : Format.formatter) (v : elem_t) : unit =
      let (c, v, loc') : elem_t = v in
      Format.fprintf fmt "(%a, %a, %a)" Cursor.pp c Value.pp v Loc.pp loc'
    in
    Format.fprintf fmt "[%a]" (Format.pp_print_list pp_elem_t) v

  let get_cursor (v : elem_t) : Cursor.t =
    let c, _, _ = v in
    c

  let get_fallthrough (v : elem_t) : Loc.t =
    let _, _, loc = v in
    loc
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
