open StdlibExt
open Basic
include Set.Make (RegId)

let pp fmt v =
  let pp_sep fmt () = Format.fprintf fmt ",@ " in
  Format.fprintf fmt "@[<hov 2>{";
  Format.pp_print_list ~pp_sep RegId.pp fmt (elements v);
  Format.fprintf fmt "}@]"
