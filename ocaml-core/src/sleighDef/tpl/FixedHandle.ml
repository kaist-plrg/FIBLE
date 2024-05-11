type t = {
  space : AddrSpace.t;
  size : Int32.t;
  offset_space : AddrSpace.t Option.t;
  offset_offset : Int64.t;
  offset_size : Int32.t;
  temp_space : AddrSpace.t Option.t;
  temp_offset : Int64.t;
}

let of_constant (v : Int64.t) : t =
  {
    space = AddrSpace.const_space;
    size = 0l;
    offset_space = None;
    offset_offset = v;
    offset_size = 0l;
    temp_space = None;
    temp_offset = 0L;
  }

let of_varnode (space : AddrSpace.t) (offset : Int64.t) (size : Int32.t) : t =
  {
    space;
    size;
    offset_space = None;
    offset_offset = offset;
    offset_size = 0l;
    temp_space = None;
    temp_offset = 0L;
  }
