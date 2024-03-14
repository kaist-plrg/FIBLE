type t = { expr : Assignable.t; output : RegId.t_full }

let pp fmt ({ expr; output } : t) =
  Format.fprintf fmt "%a = %a;" RegId.pp_full output Assignable.pp expr

let is_nop (p : t) = false
