open Basic
open Basic_collection

type t = ((Loc.t * Int64.t) * RegId.t list * RegFile.t * Value.t * Loc.t) list

let pp fmt (v : t) =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
    (fun fmt ((loc, tick), _, _, _, _) ->
      Format.fprintf fmt "%a@%Ld" Loc.pp loc tick)
    fmt v
