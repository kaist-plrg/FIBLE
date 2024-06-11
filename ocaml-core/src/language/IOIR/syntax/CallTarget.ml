open Common

module Inner = struct
  type t = { outputs : RegId.t List.t; inputs : VarNode.t List.t }

  let pp_list (fmt : Format.formatter -> 'a -> unit) =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") fmt

  let pp fmt (p : t) =
    Format.fprintf fmt "outputs = [%a]; inputs = [%a]" (pp_list RegId.pp)
      p.outputs (pp_list VarNode.pp) p.inputs
end

include Common.CallTargetF.Make (VarNode) (Inner)
