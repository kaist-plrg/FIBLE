open Common
open Sem
open Syn

let init_sp = 0x7FFFFFFFC000L

let from_signature (p : Prog.t) (args : String.t List.t) (entry : Byte8.t) :
    State.t =
  {
    sto =
      Store.init_libc_glob
        (Store.init_from_sig_main p.rom p.rspec init_sp args [])
        p.objects;
    cursor = { func = Loc.of_addr entry; tick = () };
    cont = Cont.of_func_entry_loc p (Loc.of_addr entry) |> Result.get_ok;
    stack = [];
    timestamp = ();
  }

let from_signature_libc (p : Prog.t) (args : String.t List.t) (entry : Byte8.t)
    (libc_entry : Byte8.t) : State.t =
  {
    sto =
      Store.init_libc_glob
        (Store.init_from_sig_libc p.rom p.rspec init_sp entry args [])
        p.objects;
    cursor = { func = Loc.of_addr libc_entry; tick = () };
    cont = Cont.of_func_entry_loc p (Loc.of_addr libc_entry) |> Result.get_ok;
    stack = [];
    timestamp = ();
  }

let default (p : Prog.t) (args : String.t List.t) : State.t =
  let main_func =
    List.find_opt
      (fun (x : Func.t) -> Option.equal String.equal x.nameo (Some "main"))
      p.funcs
  in
  let libc_func =
    List.find_opt
      (fun (x : Func.t) ->
        Option.equal String.equal x.nameo (Some "libc_start_main_stage2"))
      p.funcs
  in
  match (main_func, libc_func) with
  | Some main_func, Some libc_func ->
      from_signature_libc p args
        (main_func.entry |> Loc.get_addr)
        (libc_func.entry |> Loc.get_addr)
  | Some main_func, None ->
      from_signature p args (main_func.entry |> Loc.get_addr)
  | _ -> [%log error "No main function found"]
