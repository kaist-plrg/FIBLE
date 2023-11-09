open Basic

type t = { func : Loc.t; timestamp : Int64.t }

let pp fmt { func; timestamp } =
  Format.fprintf fmt "[%a@%Ld]" Loc.pp func timestamp
