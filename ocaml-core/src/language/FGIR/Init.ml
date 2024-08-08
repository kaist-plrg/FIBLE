open Common
open Sem
open Syn

let init_sp = 0x7FFFFFFFC000L

let from_signature (p : Prog.t) (args : String.t List.t) (entry : Byte8.t) :
    State.t =
  {
    sto = Store.init_from_sig p.rom p.rspec init_sp args;
    cursor = { func = Loc.of_addr entry; tick = () };
    cont = Cont.of_func_entry_loc p (Loc.of_addr entry) |> Result.get_ok;
    stack = [];
    timestamp = ();
  }

let default (p : Prog.t) (args : String.t List.t) : State.t =
  [%log debug "args: %a" (List.pp String.pp) args];
  (List.find
     (fun (x : Func.t) -> Option.equal String.equal x.nameo (Some "main"))
     p.funcs)
    .entry |> Loc.get_addr |> from_signature p args
