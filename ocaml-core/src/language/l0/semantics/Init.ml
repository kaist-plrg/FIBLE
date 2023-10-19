open Basic
open Basic_collection

let init_sp = 0x7FFFFFFFC000L

let from_signature (p : Prog.t) (a : Addr.t) : State.t =
  {
    regs =
      RegFile.add_reg RegFile.empty
        { id = RegId.Register 32L; width = 8l }
        { value = init_sp; width = 8l };
    mem =
      Memory.store_mem Memory.empty init_sp { value = 0xDEADBEEFL; width = 8l };
    pc = (a, 0);
  }
