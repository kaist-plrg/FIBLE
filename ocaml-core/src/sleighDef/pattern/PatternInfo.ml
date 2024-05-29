open Common

type t = {
  addr : Byte8.t;
  naddr : Byte8.t;
  n2addr : Byte8.t Option.t;
  umask : Int32.t;
  uoffset : Int64.t;
}
