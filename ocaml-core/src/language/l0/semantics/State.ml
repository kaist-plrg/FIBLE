open Basic

type t = {
  regs : Value.t; (* RegMap.t; *)
  mem : Value.t; (* AddrMap.t; *)
  pc : Loc.t;
  sp : Addr.t;
}
