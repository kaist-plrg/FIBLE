include Set.Make (struct
  type t = RegId.t_full

  let compare = RegId.compare_full
end)

let pp fmt v =
  let pp_sep fmt () = Format.fprintf fmt ",@ " in
  Format.fprintf fmt "@[<hov 2>{";
  Format.pp_print_list ~pp_sep RegId.pp_full fmt (elements v);
  Format.fprintf fmt "}@]"
