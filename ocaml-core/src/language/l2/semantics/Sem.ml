module NonNumericValue = NonNumericValue
module Cont = Common_language.ContF.Make (Inst) (Jmp) (Block) (Func) (Prog)
module Value = Common_language.ValueF.Make (NonNumericValue)
module TimeStamp = Common_language.Int64TimeStamp
module Cursor = Common_language.CursorF.Make (TimeStamp)
module RegFile = Common_language.RegFileF.Make (Value)
module Frame = Common_language.FrameF.Make (Value)
module LocalMemory = Common_language.LocalMemoryF.Make (Value) (Frame)
module Memory = Common_language.MemoryF.Make (Value)

module Store =
  Common_language.HighStoreF.Make (Prog) (Value) (Cursor) (RegFile) (Memory)
    (LocalMemory)

module Stack = struct
  open Basic

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
  Common_language.HighStateF.Make (Func) (Prog) (CallTarget) (JCall) (JRet)
    (TimeStamp)
    (Value)
    (Store)
    (Cont)
    (Cursor)
    (Stack)
