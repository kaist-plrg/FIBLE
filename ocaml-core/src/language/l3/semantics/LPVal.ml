open Basic

type t = { func : Loc.t; timestamp : Int64.t; offset : Int64.t }

let pp fmt { func; timestamp; offset } =
  Format.fprintf fmt "local[%a@%Ld]+%Ld" Loc.pp func timestamp offset
