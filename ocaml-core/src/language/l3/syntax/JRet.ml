open Basic

module Inner = struct
  type t = VarNode.t List.t

  let pp_list (fmt : Format.formatter -> 'a -> unit) =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") fmt

  let pp fmt (p : t) = Format.fprintf fmt "%a" (pp_list VarNode.pp) p
end

include Common_language.JRetF.Make (Inner)
