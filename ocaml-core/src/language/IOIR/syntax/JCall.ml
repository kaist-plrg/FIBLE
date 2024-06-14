open Sexplib.Std
open Common

module Inner = struct
  type t = { reserved_stack : int64; sp_diff : int64 } [@@deriving sexp]

  let pp fmt (p : t) =
    Format.fprintf fmt "reserved_stack = %Ld; sp_diff = %Ld" p.reserved_stack
      p.sp_diff
end

include Common.JCallF.Make (CallTarget) (Inner)
