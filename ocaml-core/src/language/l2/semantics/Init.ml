open StdlibExt
open Basic
open Basic_collection
open Common_language
open Sem

let from_signature (p : Prog.t) (a : Addr.t) : State.t =
  let init_sp = { SPVal.func = (a, 0); timestamp = 0L; offset = 0L } in
  {
    timestamp = 0L;
    sto =
      {
        regs =
          RegFile.add_reg (RegFile.empty p.rspec)
            { id = RegId.Register 32l; offset = 0l; width = 8l }
            (Value.sp init_sp);
        mem = Memory.from_rom p.rom;
        local = LocalMemory.empty;
      };
    cursor = { func = (a, 0); tick = 0L };
    cont = Cont.of_func_entry_loc p (a, 0) |> Result.get_ok;
    stack = [];
  }
