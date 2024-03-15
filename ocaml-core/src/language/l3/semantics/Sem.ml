module NonNumericValue = NonNumericValue
module Cont = Common.ContF.Make (Inst) (Jmp) (Block) (Func) (Prog)
module Value = Common.ValueF.Make (NonNumericValue)
module TimeStamp = Common.Int64TimeStamp
module Cursor = Common.CursorF.Make (TimeStamp)
module RegFile = Common.RegFileF.Make (Value)
module Frame = Common.FrameF.Make (Value)
module LocalMemory = Common.LocalMemoryF.Make (Value) (Frame)
module Memory = Common.MemoryF.Make (Value)

module Store =
  Common.HighStoreF.Make (Prog) (Value) (Cursor) (RegFile) (Memory) (Frame)
    (LocalMemory)

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
  Common.HighStateF.Make (Func) (Prog) (CallTarget) (JCall) (JRet) (TimeStamp)
    (Value)
    (Store)
    (Cont)
    (Cursor)
    (Stack)
    (World.Environment)
