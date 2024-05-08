type t = Unit.t

let of_constant (v : Int64.t) : t = ()

let of_varnode (space : AddrSpace.t) (offset : Int32.t) (size : Int32.t) : t =
  ()
