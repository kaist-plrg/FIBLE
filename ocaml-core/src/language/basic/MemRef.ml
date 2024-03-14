type t = R of RegId.t | ROffset of (RegId.t * int64)

let convert_varnode (v : VarNode.t) : t option =
  match v with VarNode.Register r -> Some (R r.id) | _ -> None

let convert_regid (r : RegId.t) : t = R r

let pp fmt = function
  | R u -> Format.fprintf fmt "%a" RegId.pp u
  | ROffset (u, o) -> Format.fprintf fmt "(%a + %Ld)" RegId.pp u o

let compare = compare
