open Basic
open Basic_collection
module Attr = L1Partial.State.Attr

include
  Common_language.HighStateF.Make (Prog) (Value) (Store) (Cont) (Stack) (Cursor)
    (Attr)
