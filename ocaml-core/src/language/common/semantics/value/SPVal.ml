type t = { func : Loc.t; timestamp : Int64.t; offset : Int64.t }

let pp fmt { func; timestamp; offset } =
  Format.fprintf fmt "@[[%a@%Ld]+%Ld@]" Loc.pp func timestamp offset
