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

module SCallTarget_Attr = struct
  type t = {
    outputs : Common.RegId.t List.t;
    inputs : (Common.RegId.t * Value.t) List.t;
  }

  let eval (s : Store.t) (c : CallTarget.Attr.t) : (t, String.t) Result.t =
    let* inputs =
      List.fold_right
        (fun vn lst ->
          let v =
            Common.RegIdMap.find_opt vn (Store.get_regfile s)
            |> Option.value ~default:(Value.undefined 1l)
          in
          Result.map (fun lst -> (vn, v) :: lst) lst)
        c.inputs (Ok [])
    in
    { outputs = c.outputs; inputs } |> Result.ok
end

module SCallTarget =
  Common.SCallTargetF.Make (VarNode) (CallTarget) (Value) (Store)
    (SCallTarget_Attr)

module SCall =
  Common.SCallF.Make (VarNode) (CallTarget) (JCall) (Value) (Store)
    (SCallTarget)
    (struct
      type t = JCall.Attr.t

      let eval (s : Store.t) (c : JCall.Attr.t) : (t, String.t) Result.t =
        c |> Result.ok
    end)

module STailCall_Attr = struct
  type t = {
    reserved_stack : Int64.t;
    sp_diff : Int64.t;
    returns : (Common.RegId.t * Value.t) List.t;
  }

  let eval (s : Store.t) (c : JTailCall.Attr.t) : (t, String.t) Result.t =
    let* returns =
      List.fold_right
        (fun vn lst ->
          let v =
            Common.RegIdMap.find_opt vn (Store.get_regfile s)
            |> Option.value ~default:(Value.undefined 1l)
          in
          Result.map (fun lst -> (vn, v) :: lst) lst)
        c.returns (Ok [])
    in
    { reserved_stack = c.reserved_stack; sp_diff = c.sp_diff; returns }
    |> Result.ok
end

module STailCall =
  Common.STailCallF.Make (VarNode) (CallTarget) (JTailCall) (Value) (Store)
    (SCallTarget)
    (STailCall_Attr)

module SRet =
  Common.SRetF.Make (VarNode) (JRet) (Value) (Store)
    (struct
      type t = (Common.RegId.t * Value.t) List.t

      let eval (s : Store.t) (c : JRet.Attr.t) : (t, String.t) Result.t =
        let* returns =
          List.fold_right
            (fun vn lst ->
              let v =
                Common.RegIdMap.find_opt vn (Store.get_regfile s)
                |> Option.value ~default:(Value.undefined 1l)
              in
              Result.map (fun lst -> (vn, v) :: lst) lst)
            c (Ok [])
        in
        returns |> Result.ok
    end)

module Action =
  Common.HighActionF.Make (Value) (StoreAction) (SCall) (STailCall) (SRet)

module Stack = struct
  open Common
  open Common

  type elem_t = {
    cursor : Cursor.t;
    outputs : RegId.t list;
    sregs : RegFile.t;
    saved_sp : Value.t;
    fallthrough : Loc.t;
  }

  type t = elem_t list

  let get_fallthrough (v : elem_t) = v.fallthrough
  let get_cursor (v : elem_t) = v.cursor

  let pp fmt (v : t) =
    let pp_elem fmt { cursor; outputs; sregs; saved_sp; fallthrough } =
      Format.fprintf fmt
        "@[<1>{cursor=%a;@,\
         outputs=%a;@,\
         sregs=%a;@,\
         saved_sp=%a;@,\
         fallthrough=%a}@]"
        Cursor.pp cursor
        (Format.pp_print_list RegId.pp ~pp_sep:Format.pp_print_space)
        outputs RegFile.pp sregs Value.pp saved_sp Loc.pp fallthrough
    in
    Format.fprintf fmt "[%a]" (Format.pp_print_list pp_elem) v
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
