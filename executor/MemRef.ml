type t =
 | UniqueR of int64
 | RegisterR of int64
 | UOffsetR of (int64 * int64)
 | ROffsetR of (int64 * int64)

 let convert_varnode (v: PCode.varNode): t option = 
  match v.varNode_node with
  | PCode.Unique (u) -> Some (UniqueR (u))
  | PCode.Register (r) -> Some (RegisterR (r))
  | _ -> None

let pp fmt = function 
  | UniqueR (u) -> Format.fprintf fmt "UniqueR $%Lx" u
  | RegisterR (r) -> Format.fprintf fmt "RegisterR $%Lx" r
  | UOffsetR (u, o) -> Format.fprintf fmt "UOffsetR ($%Lx, %Ld)" u o
  | ROffsetR (r, o) -> Format.fprintf fmt "ROffsetR ($%Lx, %Ld)" r o