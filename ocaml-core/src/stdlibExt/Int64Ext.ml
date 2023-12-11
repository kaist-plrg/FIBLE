include Int64

let ( let* ) = Result.bind

let cut_width (v : t) (width : int32) : int64 =
  if width > 7l then v
  else
    Int64.logand v
      (Int64.lognot (Int64.shift_left (-1L) (Int32.to_int width * 8)))

let sext (v : t) (in_width : int32) (out_width : int32) : int64 =
  let x = Int64.shift_left v (64 - (Int32.to_int in_width * 8)) in
  let x = Int64.shift_right x (64 - (Int32.to_int in_width * 8)) in
  cut_width x out_width

let zext (v : t) (in_width : int32) (out_width : int32) : int64 =
  let x = Int64.shift_left v (64 - (Int32.to_int in_width * 8)) in
  let x = Int64.shift_right_logical x (64 - (Int32.to_int in_width * 8)) in
  cut_width x out_width

let bitwidth (v : t) : int64 =
  let rec aux acc v =
    if v = 0L then acc
    else aux (Int64.add acc 1L) (Int64.shift_right_logical v 1)
  in
  aux 0L v

let bitcount (v : t) : int64 =
  let rec aux acc v =
    if v = 0L then acc
    else aux (Int64.add acc (Int64.logand v 1L)) (Int64.shift_right_logical v 1)
  in
  aux 0L v

let lift_float_uop (f : Float.t -> Float.t) (v : t) (width : int32) :
    (t, String.t) Result.t =
  if width = 4l then
    Ok
      (cut_width
         (Int64.of_int32
            (Int32.bits_of_float (f (Int32.float_of_bits (Int64.to_int32 v)))))
         width)
  else if width = 8l then
    Ok (cut_width (Int64.bits_of_float (f (Int64.float_of_bits v))) width)
  else Error "lift_float_uop: unsupported width"

let float_neg (v : t) (width : int32) : (t, String.t) Result.t =
  lift_float_uop Float.neg v width

let float_abs (v : t) (width : int32) : (t, String.t) Result.t =
  lift_float_uop Float.abs v width

let float_sqrt (v : t) (width : int32) : (t, String.t) Result.t =
  lift_float_uop Float.sqrt v width

let float_ceil (v : t) (width : int32) : (t, String.t) Result.t =
  lift_float_uop Float.ceil v width

let float_floor (v : t) (width : int32) : (t, String.t) Result.t =
  lift_float_uop Float.floor v width

let float_round (v : t) (width : int32) : (t, String.t) Result.t =
  lift_float_uop Float.round v width

let float_is_nan (v : t) (width : int32) : (t, String.t) Result.t =
  if width = 4l then
    Ok
      (if Float.is_nan (Int32.float_of_bits (Int64.to_int32 v)) then 1L else 0L)
  else if width = 8l then
    Ok (if Float.is_nan (Int64.float_of_bits v) then 1L else 0L)
  else Error "float_is_nan: unsupported width"

let int2float (v : t) (inwidth : int32) (outwidth : int32) :
    (t, String.t) Result.t =
  let fv = Int64.to_float (sext v inwidth 8l) in
  if outwidth = 4l then
    Ok (cut_width (Int64.of_int32 (Int32.bits_of_float fv)) outwidth)
  else if outwidth = 8l then Ok (cut_width (Int64.bits_of_float fv) outwidth)
  else Error "int2float: unsupported width"

let float2float (v : t) (inwidth : int32) (outwidth : int32) :
    (t, String.t) Result.t =
  let* fv =
    if inwidth = 4l then Ok (Int32.float_of_bits (Int64.to_int32 v))
    else if inwidth = 8l then Ok (Int64.float_of_bits v)
    else Error "float2float: unsupported width"
  in
  if outwidth = 4l then
    Ok (cut_width (Int64.of_int32 (Int32.bits_of_float fv)) outwidth)
  else if outwidth = 8l then Ok (cut_width (Int64.bits_of_float fv) outwidth)
  else Error "float2float: unsupported width"

let trunc (v : t) (inwidth : int32) (outwidth : int32) : (t, String.t) Result.t
    =
  let* fv =
    if inwidth = 4l then Ok (Int32.float_of_bits (Int64.to_int32 v))
    else if inwidth = 8l then Ok (Int64.float_of_bits v)
    else Error "int2float: unsupported width"
  in
  Ok (cut_width (of_float fv) outwidth)

let to_float_width (v : t) (width : int32) : (Float.t, String.t) Result.t =
  if width = 4l then Ok (Int32.float_of_bits (Int64.to_int32 v))
  else if width = 8l then Ok (Int64.float_of_bits v)
  else Error "to_float_width: unsupported width"

let of_float_width (v : Float.t) (width : int32) : (t, String.t) Result.t =
  if width = 4l then Ok (Int64.of_int32 (Int32.bits_of_float v))
  else if width = 8l then Ok (Int64.bits_of_float v)
  else Error "of_float_width: unsupported width"

let concat (v1 : t) (v2 : t) (v1width : int32) (v2width : int32) :
    (t, String.t) Result.t =
  if Int32.add v1width v2width > 8l then Error "concat: width too large"
  else Ok (Int64.logor (Int64.shift_left v1 (Int32.to_int v2width)) v2)

let carry (l : t) (r : t) (width : int32) : bool =
  if width = 8l then Int64.unsigned_compare (Int64.add l r) l < 0
  else Int64.add l r >= Int64.shift_left 1L (Int32.to_int width * 8)

let scarry (l : t) (r : t) (width : int32) : bool =
  if width = 8l then
    (l > 0L && r > 0L && Int64.add l r < 0L)
    || (l < 0L && r < 0L && Int64.add l r >= 0L)
  else
    let msbl =
      Int64.logand (Int64.shift_right l ((Int32.to_int width * 8) - 1)) 1L
    in
    let msbr =
      Int64.logand (Int64.shift_right r ((Int32.to_int width * 8) - 1)) 1L
    in
    let msb =
      Int64.logand
        (Int64.shift_right (Int64.add l r) ((Int32.to_int width * 8) - 1))
        1L
    in
    (msbl = 1L && msbr = 1L && msb = 0L) || (msbl = 0L && msbr = 0L && msb = 1L)

let sborrow (l : t) (r : t) (width : int32) : bool =
  scarry l (cut_width (Int64.neg r) width) width

let pp fmt v = Format.fprintf fmt "%Lx" v
