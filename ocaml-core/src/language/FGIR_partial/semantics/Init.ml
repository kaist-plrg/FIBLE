open Common
open Sem

let init_sp = 0x7FFFFFFFC000L

let from_signature (rspec : Int32.t Int32Map.t) (p : Prog.t) (a : Byte8.t) :
    State.t =
  {
    sto = Store.init_from_sig p.rom p.rspec init_sp;
    cursor = { func = Common.Loc.of_addr a; tick = () };
    cont = Cont.of_func_entry_loc p (Common.Loc.of_addr a) |> Result.get_ok;
    stack = [];
    timestamp = ();
  }
