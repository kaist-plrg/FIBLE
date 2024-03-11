open StdlibExt
open Basic
open Basic_collection
open Sem

let init_sp = 0x7FFFFFFFC000L

let from_signature (rspec : Int32.t Int32Map.t) (p : Prog.t) (a : Addr.t) :
    State.t =
  {
    sto = Store.init_from_sig p.rom p.rspec init_sp;
    cursor = { func = (a, 0); tick = () };
    cont = Cont.of_func_entry_loc p (a, 0) |> Result.get_ok;
    stack = [];
    timestamp = ();
  }
