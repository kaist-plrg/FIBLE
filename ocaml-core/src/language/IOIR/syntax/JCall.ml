open Sexplib.Std
open Common

module Inner = struct
  type t = { reserved_stack : int64; sp_diff : int64 }
  [@@deriving sexp, show, fields]
end

include Common.JCallF.Make (CallTarget) (Inner)
