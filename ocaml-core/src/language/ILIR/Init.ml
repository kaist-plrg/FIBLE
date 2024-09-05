open Common
open Syn
open Sem

let init_sp = 0x7FFFFFFFC000L

let from_signature (p : Prog.t) (args : String.t List.t) (entry : Byte8.t) :
    State.t =
  {
    sto =
      Store.init_libc_glob
        (Store.init_from_sig_main p.rom p.rspec init_sp args [])
        p.objects;
    pc = Loc.of_addr entry;
  }

let from_signature_libc (p : Prog.t) (args : String.t List.t) (entry : Byte8.t)
    (libc_entry : Byte8.t) : State.t =
  {
    sto =
      Store.init_libc_glob
        (Store.init_from_sig_libc p.rom p.rspec init_sp entry args [])
        p.objects;
    pc = Loc.of_addr libc_entry;
  }

let default (p : Prog.t) (args : String.t List.t) : State.t =
  let main_entry =
    List.find_opt (fun (_, name) -> String.equal name "main") p.entries
  in
  let libc_entry =
    List.find_opt
      (fun (_, name) -> String.equal name "libc_start_main_stage2")
      p.entries
  in
  match (main_entry, libc_entry) with
  | Some (mainaddr, _), Some (libcaddr, _) ->
      from_signature_libc p args mainaddr libcaddr
  | Some (mainaddr, _), None -> from_signature p args mainaddr
  | _ -> [%log error "No main function found"]
