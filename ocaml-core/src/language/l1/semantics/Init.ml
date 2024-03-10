open StdlibExt
open Basic
open Basic_collection

let init_sp = 0x7FFFFFFFC000L

let from_signature (p : Prog.t) (a : Addr.t) : State.t =
  {
    sto = Store.init_from_sig p.rom p.rspec init_sp;
    func = (a, 0);
    cont = Cont.of_func_entry_loc p (a, 0) |> Result.get_ok;
    stack = [];
  }
