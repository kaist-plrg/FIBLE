type t = { unique : int32; register : int32; const : int32; ram : int32 }

let pp fmt (s : t) =
  Format.fprintf fmt "{unique=%ld; register=%ld; const=%ld}" s.unique s.register
    s.const
