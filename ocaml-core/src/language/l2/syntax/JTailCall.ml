module Inner = struct
  type t = { reserved_stack : Int64.t; sp_diff : Int64.t }

  let pp fmt (p : t) =
    Format.fprintf fmt "Stack: %Lx; SP diff: %Lx" p.reserved_stack p.sp_diff
end

include Common.JTailCallF.Make (CallTarget) (Inner)
