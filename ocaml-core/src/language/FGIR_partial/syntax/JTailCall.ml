open Sexplib.Std

module Inner = struct
  type t = unit [@@deriving sexp]

  let pp fmt (p : t) = Format.fprintf fmt ""
end

include Common.JTailCallF.Make (CallTarget) (Inner)
