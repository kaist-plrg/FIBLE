open Common
open Sem
open Syn

let init_sp = 0x7FFFFFFC0000L

let from_signature (p : Prog.t) (args : String.t List.t) (env : String.t List.t)
    (entry : Byte8.t) : State.t =
  let stack_size = World.Util.calc_stack_size args env in
  {
    sto =
      Store.init_libc_glob
        (Store.init_from_sig_main p.rom p.rspec init_sp args env stack_size)
        p.objects 48l 56l;
    cursor = { func = Loc.of_addr entry; tick = () };
    cont = Cont.of_func_entry_loc p (Loc.of_addr entry) |> Result.get_ok;
    stack = [];
    timestamp = ();
  }

let from_signature_libc (p : Prog.t) (args : String.t List.t)
    (env : String.t List.t) (entry : Byte8.t) (libc_entry : Byte8.t) : State.t =
  let stack_size = World.Util.calc_stack_size args env in
  {
    sto =
      Store.init_libc_glob
        (Store.init_from_sig_libc p.rom p.rspec init_sp entry args env
           stack_size)
        p.objects 16l 48l;
    cursor = { func = Loc.of_addr libc_entry; tick = () };
    cont = Cont.of_func_entry_loc p (Loc.of_addr libc_entry) |> Result.get_ok;
    stack = [];
    timestamp = ();
  }

let default (p : Prog.t) (args : String.t List.t) (env : String.t List.t) :
    State.t =
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
      from_signature_libc p args env
        (main_func.entry |> Loc.get_addr)
        (libc_func.entry |> Loc.get_addr)
  | Some main_func, None ->
      from_signature p args env (main_func.entry |> Loc.get_addr)
  | _ -> [%log error "No main function found"]
