module Cont = Common.ContF.Make (Inst) (Jmp) (Block) (Func) (Prog)
module Value = Common.NumericValue
module TimeStamp = Common.UnitTimeStamp
module Cursor = Common.CursorF.Make (TimeStamp)
module RegFile = Common.RegFileF.Make (Value)
module Store = Common.LowStore

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
  Common.HighStateF.Make (Func) (Prog) (CallTarget) (JCall) (JRet) (TimeStamp)
    (Value)
    (Store)
    (Cont)
    (Cursor)
    (Stack)
    (World.Environment)
