open StdlibExt
open Basic
open Basic_collection

let init_sp = 0x7FFFFFFFC000L

let from_signature (rspec : Int32.t Int32Map.t) (p : Prog.t) (a : Addr.t) :
    State.t =
  {
    sto =
      {
        regs =
          RegFile.add_reg (RegFile.empty rspec)
            { id = RegId.Register 32l; offset = 0l; width = 8l }
            (Value.of_int64 init_sp 8l);
        mem =
          Memory.store_mem (Memory.from_rom p.rom) init_sp
            (Value.of_int64 0xDEADBEEFL 8l);
      };
    func = (a, 0);
    cont = Cont.of_func_entry_loc p (a, 0) |> Result.get_ok;
    stack = [];
  }
