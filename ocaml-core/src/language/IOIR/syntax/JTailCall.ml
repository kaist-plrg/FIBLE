open Sexplib.Std
open Common

module Inner = struct
  type t = { reserved_stack : int64; sp_diff : int64; returns : VarNode.t list }
  [@@deriving sexp]

  let pp_list (fmt : Format.formatter -> 'a -> unit) =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") fmt

  let pp fmt (p : t) =
    Format.fprintf fmt "reserved_stack = %Ld; sp_diff = %Ld; returns = [%a]"
      p.reserved_stack p.sp_diff (pp_list VarNode.pp) p.returns
end

include Common.JTailCallF.Make (CallTarget) (Inner)
