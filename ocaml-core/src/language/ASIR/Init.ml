open Common
open Syn
open Sem

let from_signature (p : Prog.t) (args : String.t List.t) (env : String.t List.t)
    (a : Byte8.t) : State.t =
  let stack_size = World.Util.calc_stack_size args env in
  let init_sp =
    { SPVal.func = Loc.of_addr a; timestamp = 0L; multiplier = 1L; offset = 0L }
  in
  let f = Prog.get_func_opt p (Loc.of_addr a) |> Option.get in
  {
    timestamp = 0L;
    sto =
      Store.init_libc_glob
        (Store.init_from_sig_main p.rom p.rspec
           (Loc.of_addr a, 0L)
           (Frame.empty (fst f.attr.sp_boundary)
              (Int64.add (snd f.attr.sp_boundary) stack_size))
           (Value.sp init_sp) args env stack_size)
        p.objects 48l 56l;
    cursor = { func = Loc.of_addr a; tick = 0L };
    cont = Cont.of_func_entry_loc p (Loc.of_addr a) |> Result.get_ok;
    stack = [];
  }

let from_signature_libc (p : Prog.t) (args : String.t List.t)
    (env : String.t List.t) (a : Byte8.t) (libc_a : Byte8.t) : State.t =
  let stack_size = World.Util.calc_stack_size args env in
  let init_sp =
    {
      SPVal.func = Loc.of_addr libc_a;
      timestamp = 0L;
      multiplier = 1L;
      offset = 0L;
    }
  in
  let f = Prog.get_func_opt p (Loc.of_addr libc_a) |> Option.get in
  {
    timestamp = 0L;
    sto =
      Store.init_libc_glob
        (Store.init_from_sig_libc p.rom p.rspec
           (Loc.of_addr libc_a, 0L)
           (Frame.empty (fst f.attr.sp_boundary)
              (Int64.add (snd f.attr.sp_boundary) stack_size))
           (Value.sp init_sp) a args env stack_size)
        p.objects 16l 48l;
    cursor = { func = Loc.of_addr libc_a; tick = 0L };
    cont = Cont.of_func_entry_loc p (Loc.of_addr libc_a) |> Result.get_ok;
    stack = [];
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
