include Int64

let cut_width (v : t) (width : int32) : int64 =
  if width > 7l then v
  else
    Int64.logand v
      (Int64.lognot (Int64.shift_left (-1L) (Int32.to_int width * 8)))

let sext (v : t) (in_width : int32) (out_width : int32) : int64 =
  let x = Int64.shift_left v (64 - (Int32.to_int in_width * 8)) in
  let x = Int64.shift_right x (64 - (Int32.to_int in_width * 8)) in
  cut_width x out_width

let pp fmt v = Format.fprintf fmt "%Lx" v
