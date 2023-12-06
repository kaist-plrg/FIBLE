open StdlibExt
open Basic
open Basic_collection

let from_signature (rspec : Int32.t Int32Map.t) (p : Prog.t) (a : Addr.t) :
    State.t =
  let init_sp = { SPVal.func = (a, 0); timestamp = 0L; offset = 0L } in
  {
    timestamp = 0L;
    sto =
      {
        regs =
          RegFile.add_reg (RegFile.empty rspec)
            { id = RegId.Register 32l; offset = 0l; width = 8l }
            (Value.NonNum (SP init_sp));
        mem = Memory.from_rom p.rom;
        local =
          LocalMemory.store_mem LocalMemory.empty init_sp
            (Value.Num { value = 0xDEADBEEFL; width = 8l });
      };
    func = ((a, 0), 0L);
    cont = Cont.of_func_entry_loc p (a, 0) |> Result.get_ok;
    stack = [];
  }
