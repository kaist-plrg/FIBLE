open Common

type t = SP of SPVal.t | Undef of UndefVal.t | Reg of RegId.t

let pp fmt = function
  | SP sp -> SPVal.pp fmt sp
  | Undef i -> UndefVal.pp fmt i
  | Reg r -> Format.fprintf fmt "reg_%a" RegId.pp r

let compare (a : t) (b : t) =
  match (a, b) with
  | SP a, SP b -> SPVal.compare a b
  | Undef a, Undef b -> UndefVal.compare a b
  | Reg a, Reg b -> RegId.compare a b
  | SP _, _ -> -1
  | Undef _, SP _ -> 1
  | Undef _, _ -> -1
  | Reg _, _ -> 1

let eval_uop (u : Uop.t) (v : t) (outwidth : Int32.t) :
    (NumericValue.t, t) Either.t =
  match (u, v) with
  | Uop.Uint_2comp, SP o ->
      Right
        (SP
           {
             o with
             multiplier = Int64.neg o.multiplier;
             offset = Int64.neg o.offset;
           })
  | Uop.Uint_negate, SP o ->
      Right
        (SP
           {
             o with
             multiplier = Int64.neg o.multiplier;
             offset = Int64.sub (-1L) o.offset;
           })
  | _ -> Right (Undef (UndefVal.of_width outwidth))

let add_sp_arith (o : SPVal.t) (v : Int64.t) : t =
  SP
    {
      timestamp = o.timestamp;
      func = o.func;
      multiplier = o.multiplier;
      offset = Int64.add o.offset v;
      width = o.width;
    }

let get_mask (v : Int64.t) : Int64.t =
  if Int64.equal (Int64.logand 0xFFFFFFFFFFFFFFF0L v) 0xFFFFFFFFFFFFFFF0L then
    match Int64.logand 0xFL v with
    | 0x0L -> -0x10L
    | 0x8L -> -0x8L
    | 0xCL -> -0x4L
    | 0xEL -> -0x2L
    | 0xFL -> -0x1L
    | _ -> 0L
  else if Int64.equal (Int64.logand 0xFFFFFFFFFFFFFFF0L v) 0x0L then
    match Int64.logand 0xFL v with
    | 0xFL -> 0x10L
    | 0x7L -> 0x8L
    | 0x3L -> 0x4L
    | 0x1L -> 0x2L
    | 0x0L -> 0x1L
    | _ -> 0L
  else 0L

let eval_bop (b : Bop.t)
    (vs : (t * t, t * NumericValue.t, NumericValue.t * t) Either3.t)
    (outwidth : Int32.t) : (NumericValue.t, t) Either.t =
  match (b, vs) with
  | Bop.Bint_equal, Second (Undef a, v) | Bop.Bint_equal, Third (v, Undef a) ->
      if
        NumericValue.isZero v |> Option.value ~default:false
        && UndefVal.is_must_nonzero a
      then Left (NumericValue.of_int64 0L 1l)
      else Right (Undef (UndefVal.of_width 1l))
  | Bop.Bint_notequal, Second (Undef a, v)
  | Bop.Bint_notequal, Third (v, Undef a) ->
      if
        NumericValue.isZero v |> Option.value ~default:false
        && UndefVal.is_must_nonzero a
      then Left (NumericValue.of_int64 1L 1l)
      else Right (Undef (UndefVal.of_width 1l))
  | _, Second (Undef _, _) | _, Third (_, Undef _) ->
      Right (Undef (UndefVal.of_width outwidth))
  | Bop.Bint_add, Second (SP o, lv) | Bop.Bint_add, Third (lv, SP o) -> (
      match NumericValue.value_64 lv with
      | Ok ln ->
          Right (add_sp_arith o (Int64.sext ln (NumericValue.width lv) 8l))
      | Error _ -> Right (Undef (UndefVal.of_width outwidth)))
  | Bop.Bint_sub, Second (SP o, rv) -> (
      match NumericValue.value_64 rv with
      | Ok rn ->
          Right
            (add_sp_arith o
               (Int64.neg (Int64.sext rn (NumericValue.width rv) 8l)))
      | Error _ -> Right (Undef (UndefVal.of_width outwidth)))
  | Bop.Bint_sub, Third (rv, SP o) -> (
      match NumericValue.value_64 rv with
      | Ok rn ->
          Right
            (SP
               {
                 o with
                 multiplier = Int64.neg o.multiplier;
                 offset = Int64.sub rn o.offset;
               })
      | Error _ -> Right (Undef (UndefVal.of_width outwidth)))
  | Bop.Bint_sub, First (SP o1, SP o2) ->
      if (o1.timestamp, o1.func) = (o2.timestamp, o2.func) then
        if o1.multiplier = o2.multiplier then
          Left (NumericValue.of_int64 (Int64.sub o1.offset o2.offset) outwidth)
        else
          Right
            (SP
               {
                 timestamp = o1.timestamp;
                 func = o1.func;
                 multiplier = Int64.sub o1.multiplier o2.multiplier;
                 offset = Int64.sub o1.offset o2.offset;
                 width = Int32.max o1.width o2.width;
               })
      else Right (Undef (UndefVal.of_width ~must_nonzero:true outwidth))
  | Bop.Bint_sub, First (v1, v2) ->
      if (match v1 with Undef _ -> false | _ -> true) && v1 = v2 then
        Left (NumericValue.zero outwidth)
      else Right (Undef (UndefVal.of_width outwidth))
  | Bop.Bint_mult, Second (SP o, lv) | Bop.Bint_mult, Third (lv, SP o) -> (
      match NumericValue.value_64 lv with
      | Ok rn ->
          Right
            (SP
               {
                 o with
                 multiplier = Int64.mul o.multiplier rn;
                 offset = Int64.mul o.offset rn;
               })
      | Error _ -> Right (Undef (UndefVal.of_width outwidth)))
  | Bop.Bint_xor, First (v1, v2) ->
      if (match v1 with Undef _ -> false | _ -> true) && compare v1 v2 = 0
      then Left (NumericValue.zero outwidth)
      else Right (Undef (UndefVal.of_width outwidth))
  | Bop.Bint_and, First (v1, v2) ->
      if (match v1 with Undef _ -> false | _ -> true) && compare v1 v2 = 0
      then Right v1
      else Right (Undef (UndefVal.of_width outwidth))
  | Bop.Bint_and, Second (SP o, lv) | Bop.Bint_and, Third (lv, SP o) -> (
      match NumericValue.value_64 lv with
      | Ok rn ->
          let masked = get_mask rn in
          if Int64.compare masked 0L < 0 then
            Right
              (SP
                 {
                   o with
                   offset =
                     Int64.sub o.offset
                       (Int64.unsigned_rem
                          (Int64.add (Int64.mul 8L o.multiplier) o.offset)
                          (Int64.neg masked));
                 })
          else if Int64.compare masked 0L > 0 then
            Left
              (NumericValue.of_int64
                 (Int64.unsigned_rem
                    (Int64.add (Int64.mul 8L o.multiplier) o.offset)
                    masked)
                 outwidth)
          else Right (Undef (UndefVal.of_width outwidth))
      | _ -> Right (Undef (UndefVal.of_width outwidth)))
  | Bop.Bint_rem, Second (SP o, lv) | Bop.Bint_rem, Third (lv, SP o) -> (
      match NumericValue.value_64 lv with
      | Ok 16L ->
          Left
            (NumericValue.of_int64
               (Int64.unsigned_rem
                  (Int64.add (Int64.mul 8L o.multiplier) o.offset)
                  16L)
               outwidth)
      | Ok 8L ->
          Left (NumericValue.of_int64 (Int64.unsigned_rem o.offset 8L) outwidth)
      | Ok 4L ->
          Left (NumericValue.of_int64 (Int64.unsigned_rem o.offset 4L) outwidth)
      | Ok 2L ->
          Left (NumericValue.of_int64 (Int64.unsigned_rem o.offset 2L) outwidth)
      | Ok 1L ->
          Left (NumericValue.of_int64 (Int64.unsigned_rem o.offset 1L) outwidth)
      | _ -> Right (Undef (UndefVal.of_width outwidth)))
  | Bop.Bint_equal, First (SP o1, SP o2) ->
      if SPVal.compare o1 o2 = 0 then Left (NumericValue.of_int64 1L 1l)
      else Left (NumericValue.of_int64 0L 1l)
  | Bop.Bint_equal, Second _ | Bop.Bint_equal, Third _ ->
      Left (NumericValue.of_int64 0L 1l)
  | Bop.Bint_notequal, First (SP o1, SP o2) ->
      if (o1.timestamp, o1.func, o1.offset) = (o2.timestamp, o2.func, o2.offset)
      then Left (NumericValue.of_int64 0L 1l)
      else Left (NumericValue.of_int64 1L 1l)
  | Bop.Bint_notequal, Second _ | Bop.Bint_notequal, Third _ ->
      Left (NumericValue.of_int64 1L 1l)
  | Bop.Bint_sless, First (SP o1, SP o2) ->
      if
        Int64.equal o1.timestamp o2.timestamp
        && Loc.equal o1.func o2.func
        && Int64.equal o1.multiplier o2.multiplier
      then
        Left
          (NumericValue.of_int64 (if o1.offset < o2.offset then 1L else 0L) 8l)
      else Right (Undef (UndefVal.of_width outwidth))
  | Bop.Bint_less, First (SP o1, SP o2) ->
      if
        Int64.equal o1.timestamp o2.timestamp
        && Loc.equal o1.func o2.func
        && Int64.equal o1.multiplier o2.multiplier
      then
        Left
          (NumericValue.of_int64 (if o1.offset < o2.offset then 1L else 0L) 8l)
      else Right (Undef (UndefVal.of_width outwidth))
  | _ -> Right (Undef (UndefVal.of_width outwidth))

let width (v : t) : Int32.t =
  match v with
  | Undef udf -> UndefVal.width udf
  | SP { width; _ } -> width
  | _ -> 8l

let undefined (width : Int32.t) : t = Undef (UndefVal.of_width width)
let sp (v : SPVal.t) : t = SP v

let get_sp (v : t) : SPVal.t Option.t =
  match v with SP v -> Some v | _ -> None
