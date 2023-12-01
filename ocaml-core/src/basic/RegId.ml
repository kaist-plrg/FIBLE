type t = Unique of int64 | Register of int64
type t_width = { id : t; width : int32 }

let width v = v.width

let pp (fmt : Format.formatter) (v : t) =
  match v with
  | Register n -> Format.fprintf fmt "$%Ld" n
  | Unique n -> Format.fprintf fmt "#%Ld" n

let pp_width (fmt : Format.formatter) (v : t_width) =
  Format.fprintf fmt "%a:%ld" pp v.id v.width

let compare (a : t) (b : t) =
  match (a, b) with
  | Register a, Register b -> Int64.compare a b
  | Unique a, Unique b -> Int64.compare a b
  | Register _, Unique _ -> -1
  | Unique _, Register _ -> 1
