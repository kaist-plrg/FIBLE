open StdlibExt
open Basic

include
  Common_language.Block.Make
    (struct
      type t = Inst.t_full

      let pp = Inst.pp_full
    end)
    (struct
      type t = Jmp.t_full

      let pp = Jmp.pp_full
      let succ = Jmp.succ_full
    end)
