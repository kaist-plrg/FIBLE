open Common
open Sem

let init_sp = 0x7FFFFFFFC000L

let from_signature (p : Prog.t) (a : Byte8.t) : State.t =
  { sto = Store.init_from_sig p.rom p.rspec init_sp; pc = Loc.of_addr a }
