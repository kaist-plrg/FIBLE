open Basic
open Basic_collection

type t =
  ((Loc.t * Int64.t) * RegId.t list * RegFile.t * Value.t * Loc.t) list
