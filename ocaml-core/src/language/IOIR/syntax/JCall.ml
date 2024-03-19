open Common

module Inner = struct
  type t = { reserved_stack : Int64.t; sp_diff : Int64.t }

  let pp fmt (p : t) =
    Format.fprintf fmt "reserved_stack = %Ld; sp_diff = %Ld" p.reserved_stack
      p.sp_diff
end

include Common.JCallF.Make (CallTarget) (Inner)
