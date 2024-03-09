open Basic
open Basic_collection

type t = {
  timestamp : Int64.t;
  sto : Store.t;
  cont : Cont.t;
  stack : Stack.t;
  func : Loc.t * Int64.t;
}

let pp fmt (s : t) : unit =
  Format.fprintf fmt "func: %a@%Ld\n cont: %a\nstack: %a\n" Loc.pp (fst s.func)
    (snd s.func) Cont.pp s.cont Stack.pp s.stack
