type t = {
  func : Loc.t;
  timestamp : Int64.t;
  multiplier : Int64.t;
  offset : Int64.t;
}

let compare (a : t) (b : t) =
  let c = Loc.compare a.func b.func in
  if c = 0 then
    let c = Int64.compare a.timestamp b.timestamp in
    if c = 0 then
      let c = Int64.compare a.multiplier b.multiplier in
      if c = 0 then Int64.compare a.offset b.offset else c
    else c
  else c

let pp fmt { func; timestamp; offset } =
  Format.fprintf fmt "@[[%a@%Ld]+%Ld@]" Loc.pp func timestamp offset
