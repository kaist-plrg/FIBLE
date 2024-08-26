open Common
open Syn
open Sem

let init_sp = 0x7FFFFFFFC000L

let from_signature (p : Prog.t) (args : String.t List.t) (entry : Byte8.t) :
    State.t =
  {
    sto =
      Store.init_libc_glob
        (Store.init_from_sig p.rom p.rspec init_sp args)
        p.objects;
    pc = Loc.of_addr entry;
  }

let default (p : Prog.t) (args : String.t List.t) : State.t =
  fst
    (List.find
       (fun ((x, y) : Int64.t * String.t) -> String.equal y "main")
       p.entries)
  |> from_signature p args
