open StdlibExt
open Common
open Sem

let from_signature (p : Prog.t) (a : Addr.t) : State.t =
  let init_sp = { SPVal.func = (a, 0); timestamp = 0L; offset = 0L } in
  let f = Prog.get_func_opt p (a, 0) |> Option.get in
  let local =
    LocalMemory.add
      ((a, 0), 0L)
      (Frame.empty (fst f.sp_boundary) (snd f.sp_boundary))
      LocalMemory.empty
  in
  {
    timestamp = 0L;
    sto =
      {
        regs =
          RegFile.add_reg (RegFile.empty p.rspec)
            { id = RegId.Register 32l; offset = 0l; width = 8l }
            (Value.sp init_sp);
        mem = Memory.from_rom p.rom;
        local;
      };
    cursor = { func = (a, 0); tick = 0L };
    cont = Cont.of_func_entry_loc p (a, 0) |> Result.get_ok;
    stack = [];
  }

let default (p : Prog.t) : State.t =
  (List.find
     (fun (x : Func.t) ->
       String.equal (Option.value x.nameo ~default:"") "main")
     p.funcs)
    .entry |> fst |> from_signature p
