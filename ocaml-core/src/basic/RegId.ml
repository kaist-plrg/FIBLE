type t = Unique of int64 | Register of int64
type t_width = { id : t; width : int32 }

let width v = v.width

let pp (fmt : Format.formatter) (v : t) =
  match v with
  | Register n -> Format.fprintf fmt "$%Ld" n
  | Unique n -> Format.fprintf fmt "#%Ld" n

let pp_width (fmt : Format.formatter) (v : t_width) =
  Format.fprintf fmt "%a:%ld" pp v.id v.width

let compare = compare
