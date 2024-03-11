open Basic
open Basic_collection

module Inner = struct
  type t = { sto : Store.t; cont : Cont.t; stack : Stack.t; func : Loc.t }

  let get_store (s : t) : Store.t = s.sto
  let set_store (s : t) (sto : Store.t) : t = { s with sto }
  let get_cont (s : t) : Cont.t = s.cont
  let set_cont (s : t) (cont : Cont.t) : t = { s with cont }
  let get_func_loc (s : t) : Loc.t = s.func

  let pp fmt (s : t) : unit =
    Format.fprintf fmt "func: %a\n cont: %a\n" Loc.pp s.func Cont.pp s.cont
end

include Inner

include
  Common_language.StateIntraJumpF.Make (Prog) (Value) (Store) (Cont) (Inner)
