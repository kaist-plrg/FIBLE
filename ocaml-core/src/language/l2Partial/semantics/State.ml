open Basic
open Basic_collection

module Attr = struct
  type t = { timestamp : Int64.t }

  let pp fmt (a : t) : unit = Format.fprintf fmt "timestamp: %Ld" a.timestamp
end

include
  Common_language.HighStateF.Make (Prog) (Value) (Store) (Cont) (Stack) (Cursor)
    (Attr)
