type t = {
  func : Loc.t;
  timestamp : Int64.t;
  multiplier : Int64.t;
  bitshift : Int64.t;
  offset : Int64.t;
  width : Int32.t;
}

let compare (a : t) (b : t) =
  let c = Loc.compare a.func b.func in
  if c = 0 then
    let c = Int64.compare a.timestamp b.timestamp in
    if c = 0 then
      let c = Int64.compare a.multiplier b.multiplier in
      if c = 0 then
        let c = Int64.compare a.bitshift b.bitshift in
        if c = 0 then
          let c = Int64.compare a.offset b.offset in
          if c = 0 then Int32.compare a.width b.width else c
        else c
      else c
    else c
  else c

let pp fmt { func; timestamp; offset; width } =
  Format.fprintf fmt "@[[%a@%Ld]+%Ld@]:%ld" Loc.pp func timestamp offset width
