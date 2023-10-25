type tag =
  | T64
  | T32
  | T16
  | T8
  | TString
  | TFloat
  | TDouble
  | TList of tag
  | TVoid

type func_sig = { params : tag list; result : tag }

type t =
  | V64 of Int64.t
  | V32 of Int32.t
  | V16 of Int32.t
  | V8 of Char.t
  | VString of String.t
  | VFloat of Float.t
  | VDouble of Float.t
  | VList of tag * t list
  | VUnit
