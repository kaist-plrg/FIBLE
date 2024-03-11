module Cont = Common_language.ContF.Make (Inst) (Jmp) (Block) (Func) (Prog)
module Value = Common_language.NumericValue
module TimeStamp = Common_language.UnitTimeStamp
module Cursor = Common_language.CursorF.Make (TimeStamp)
module RegFile = Common_language.RegFileF.Make (Value)
module Store = Common_language.LowStore

module Stack = struct
  open Basic

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
  Common_language.HighStateF.Make (Func) (Prog) (CallTarget) (JCall) (JRet)
    (TimeStamp)
    (Value)
    (Store)
    (Cont)
    (Cursor)
    (Stack)
