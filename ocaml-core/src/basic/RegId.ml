type regid = Unique of int64 | Register of int64
type t = { id : regid; width : int32 }

let width v = v.width

let pp (fmt : Format.formatter) (v : t) =
  match v.id with
  | Register n -> Format.fprintf fmt "$%Ld:%ld" n v.width
  | Unique n -> Format.fprintf fmt "#%Ld:%ld" n v.width

let compare = compare
