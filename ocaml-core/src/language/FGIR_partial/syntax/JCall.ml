module Inner = struct
  type t = Unit.t

  let pp fmt (p : t) = Format.fprintf fmt ""
end

include Common.JCallF.Make (CallTarget) (Inner)
