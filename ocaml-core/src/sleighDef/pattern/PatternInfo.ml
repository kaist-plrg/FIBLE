open Common

type t = {
  addr : Addr.t;
  naddr : Addr.t;
  n2addr : Addr.t Option.t;
  umask : Int32.t;
  uoffset : Int64.t;
}
