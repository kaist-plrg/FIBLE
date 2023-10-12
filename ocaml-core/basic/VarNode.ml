type varNodeI =
  | Unique of int64
  | Register of int64
  | Const of int64
  | Ram of int64

type t = { varNode_node : varNodeI; varNode_width : int32 }

let pp (fmt : Format.formatter) (v : t) =
  match v.varNode_node with
  | Register n -> Format.fprintf fmt "$%Ld:%ld" n v.varNode_width
  | Unique n -> Format.fprintf fmt "#%Ld:%ld" n v.varNode_width
  | Const n -> Format.fprintf fmt "%Ld:%ld" n v.varNode_width
  | Ram n -> Format.fprintf fmt "*%Ld:%ld" n v.varNode_width

  let compare = compare
