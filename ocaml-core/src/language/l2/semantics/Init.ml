open Basic
open Basic_collection

let from_signature (p : Prog.t) (a : Addr.t) : State.t =
  let init_sp = { SPVal.func = (a, 0); timestamp = 0L; offset = 0L } in
  {
    timestamp = 0L;
    sto =
      {
        regs =
          RegFile.add_reg RegFile.empty
            { id = RegId.Register 32L; width = 8l }
            (Value.SP init_sp);
        mem = Memory.empty;
        local =
          LocalMemory.store_mem LocalMemory.empty init_sp
            (Value.Num { value = 0xDEADBEEFL; width = 8l });
      };
    func = (a, 0), 0L;
    cont = Cont.of_func_entry_loc p (a, 0) |> Result.get_ok;
    stack = [];
  }
