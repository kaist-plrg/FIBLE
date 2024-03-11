open Basic
open Basic_collection

module Attr = struct
  type t = Unit.t

  let pp fmt (v : t) = Format.fprintf fmt ""
end

include
  Common_language.HighStateF.Make (Prog) (Value) (Store) (Cont) (Stack) (Cursor)
    (Attr)
