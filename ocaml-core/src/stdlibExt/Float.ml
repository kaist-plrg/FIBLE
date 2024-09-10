type t = F32 of Float32.t | F64 of Float64.t | F80 of Float80.t

let equal (x : t) (y : t) : bool =
  match (x, y) with
  | F32 x, F32 y -> Float32.equal x y
  | F64 x, F64 y -> Float64.equal x y
  | F80 x, F80 y -> Float80.equal x y
  | _, _ -> false

let compare (x : t) (y : t) : Int.t =
  match (x, y) with
  | F32 x, F32 y -> Float32.compare x y
  | F64 x, F64 y -> Float64.compare x y
  | F80 x, F80 y -> Float80.compare x y
  | F32 _, _ -> -1
  | _, F32 _ -> 1
  | F64 _, _ -> -1
  | _, F64 _ -> 1

let of_bytes (b : Bytes.t) : t =
  if Bytes.length b = 4 then F32 (Float32.of_bytes_raw b)
  else if Bytes.length b = 8 then F64 (Float64.of_bytes_raw b)
  else if Bytes.length b = 10 then F80 (Float80.of_bytes_raw b)
  else invalid_arg "Float.of_bytes"

let of_string (s : String.t) : t =
  if String.length s = 4 then F32 (Float32.of_string_raw s)
  else if String.length s = 8 then F64 (Float64.of_string_raw s)
  else if String.length s = 10 then F80 (Float80.of_string_raw s)
  else invalid_arg "Float.of_string"

let read (s : String.t) (width : Int.t) : t =
  if width = 4 then F32 (Float32.read s)
  else if width = 8 then F64 (Float64.read s)
  else if width = 10 then F80 (Float80.read s)
  else invalid_arg "Float.read"

let show (v : t) : String.t =
  match v with
  | F32 v -> Float32.show v
  | F64 v -> Float64.show v
  | F80 v -> Float80.show v

let pp (fmt : Format.formatter) (v : t) : unit =
  Format.fprintf fmt "%s" (show v)

let neg (v : t) : t =
  match v with
  | F32 v -> F32 (Float32.neg v)
  | F64 v -> F64 (Float64.neg v)
  | F80 v -> F80 (Float80.neg v)

let abs (v : t) : t =
  match v with
  | F32 v -> F32 (Float32.abs v)
  | F64 v -> F64 (Float64.abs v)
  | F80 v -> F80 (Float80.abs v)

let sqrt (v : t) : t =
  match v with
  | F32 v -> F32 (Float32.sqrt v)
  | F64 v -> F64 (Float64.sqrt v)
  | F80 v -> F80 (Float80.sqrt v)

let ceil (v : t) : t =
  match v with
  | F32 v -> F32 (Float32.ceil v)
  | F64 v -> F64 (Float64.ceil v)
  | F80 v -> F80 (Float80.ceil v)

let floor (v : t) : t =
  match v with
  | F32 v -> F32 (Float32.floor v)
  | F64 v -> F64 (Float64.floor v)
  | F80 v -> F80 (Float80.floor v)

let round (v : t) : t =
  match v with
  | F32 v -> F32 (Float32.round v)
  | F64 v -> F64 (Float64.round v)
  | F80 v -> F80 (Float80.round v)

let is_nan (v : t) : Bool.t =
  match v with
  | F32 v -> Float32.is_nan v
  | F64 v -> Float64.is_nan v
  | F80 v -> Float80.is_nan v

let trunc (v : t) : Z.t =
  (match v with
  | F32 v -> Float32.trunc v
  | F64 v -> Float64.trunc v
  | F80 v -> Float80.trunc v)
  |> Z.of_int64

let add (x : t) (y : t) : t =
  match (x, y) with
  | F32 x, F32 y -> F32 (Float32.add x y)
  | F64 x, F64 y -> F64 (Float64.add x y)
  | F80 x, F80 y -> F80 (Float80.add x y)
  | _, _ -> invalid_arg "Float.add"

let sub (x : t) (y : t) : t =
  match (x, y) with
  | F32 x, F32 y -> F32 (Float32.sub x y)
  | F64 x, F64 y -> F64 (Float64.sub x y)
  | F80 x, F80 y -> F80 (Float80.sub x y)
  | _, _ -> invalid_arg "Float.sub"

let mul (x : t) (y : t) : t =
  match (x, y) with
  | F32 x, F32 y -> F32 (Float32.mul x y)
  | F64 x, F64 y -> F64 (Float64.mul x y)
  | F80 x, F80 y -> F80 (Float80.mul x y)
  | _, _ -> invalid_arg "Float.mul"

let div (x : t) (y : t) : t =
  match (x, y) with
  | F32 x, F32 y -> F32 (Float32.div x y)
  | F64 x, F64 y -> F64 (Float64.div x y)
  | F80 x, F80 y -> F80 (Float80.div x y)
  | _, _ -> invalid_arg "Float.div"
(*
   let lift_float_uop (f : Float.t -> Float.t) (v : t) (width : int32) :
       (t, String.t) Result.t =
     if width = 4l then
       Ok
         (cut_width
            (Stdlib.Int64.of_int32
               (Int32.bits_of_float
                  (f (Int32.float_of_bits (Stdlib.Int64.to_int32 v)))))
            width)
     else if width = 8l then
       Ok
         (cut_width
            (Stdlib.Int64.bits_of_float (f (Stdlib.Int64.float_of_bits v)))
            width)
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
         (if Float.is_nan (Int32.float_of_bits (Stdlib.Int64.to_int32 v)) then 1L
          else 0L)
     else if width = 8l then
       Ok (if Float.is_nan (Stdlib.Int64.float_of_bits v) then 1L else 0L)
     else Error "float_is_nan: unsupported width"

   let int2float (v : t) (inwidth : int32) (outwidth : int32) :
       (t, String.t) Result.t =
     let fv = Stdlib.Int64.to_float (sext v inwidth 8l) in
     if outwidth = 4l then
       Ok (cut_width (Stdlib.Int64.of_int32 (Int32.bits_of_float fv)) outwidth)
     else if outwidth = 8l then
       Ok (cut_width (Stdlib.Int64.bits_of_float fv) outwidth)
     else Error "int2float: unsupported width"

   let float2float (v : t) (inwidth : int32) (outwidth : int32) :
       (t, String.t) Result.t =
     let* fv =
       if inwidth = 4l then Ok (Int32.float_of_bits (Stdlib.Int64.to_int32 v))
       else if inwidth = 8l then Ok (Stdlib.Int64.float_of_bits v)
       else Error "float2float: unsupported width"
     in
     if outwidth = 4l then
       Ok (cut_width (Stdlib.Int64.of_int32 (Int32.bits_of_float fv)) outwidth)
     else if outwidth = 8l then
       Ok (cut_width (Stdlib.Int64.bits_of_float fv) outwidth)
     else Error "float2float: unsupported width"

   let to_float_width (v : t) (width : int32) : (Float.t, String.t) Result.t =
     if width = 4l then Ok (Int32.float_of_bits (Stdlib.Int64.to_int32 v))
     else if width = 8l then Ok (Stdlib.Int64.float_of_bits v)
     else Error "to_float_width: unsupported width"

   let of_float_width (v : Float.t) (width : int32) : (t, String.t) Result.t =
     if width = 4l then Ok (Stdlib.Int64.of_int32 (Int32.bits_of_float v))
     else if width = 8l then Ok (Stdlib.Int64.bits_of_float v)
     else Error "of_float_width: unsupported width"
*)
