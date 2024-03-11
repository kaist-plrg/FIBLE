open StdlibExt
open Basic
open Basic_collection
open Sem

let init_sp = 0x7FFFFFFFC000L

let from_signature (p : Prog.t) (a : Addr.t) : State.t =
  { sto = Store.init_from_sig p.rom p.rspec init_sp; pc = (a, 0) }
