open Basic
open Basic_collection

type t = { sto : Store.t; cont : Cont.t; stack : Stack.t; func : Loc.t }

let pp fmt (s : t) : unit =
  Format.fprintf fmt "func: %a\n cont: %a\n" Loc.pp s.func Cont.pp s.cont
