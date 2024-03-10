open Basic

module Inner = struct
  type t = {
    reserved_stack : Int64.t;
    sp_diff : Int64.t;
    returns : VarNode.t List.t;
  }

  let pp_list (fmt : Format.formatter -> 'a -> unit) =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") fmt

  let pp fmt (p : t) =
    Format.fprintf fmt "reserved_stack = %Ld; sp_diff = %Ld; returns = [%a]"
      p.reserved_stack p.sp_diff (pp_list VarNode.pp) p.returns
end

include Common_language.JTailCallF.Make (CallTarget) (Inner)
