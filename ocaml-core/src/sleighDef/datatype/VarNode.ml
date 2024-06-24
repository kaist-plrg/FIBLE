type t = { space : Int32.t; size : Int32.t; offset : Int64.t }

let make space size offset = { space; size; offset }

let pp fmt (v : t) =
  Format.fprintf fmt "{space=%ld; offset=%Ld; size=%ld}" v.space v.offset v.size
