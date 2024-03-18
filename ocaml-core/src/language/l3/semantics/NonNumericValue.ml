open StdlibExt
open Common

type t = SP of SPVal.t | Undef of Int32.t

let pp fmt = function
  | SP sp -> SPVal.pp fmt sp
  | Undef i -> Format.fprintf fmt "undef_%ld" i

let eval_uop (u : Uop.t) (v : t) (outwidth : Int32.t) :
    (NumericValue.t, t) Either.t =
  Right (Undef outwidth)

let eval_sp_arith (o : SPVal.t) (v : Int64.t) : t =
  SP
    {
      timestamp = o.timestamp;
      func = o.func;
      multiplier = o.multiplier;
      offset = Int64.add o.offset v;
    }

let eval_bop (b : Bop.t)
    (vs : (t * t, t * NumericValue.t, NumericValue.t * t) Either3.t)
    (outwidth : Int32.t) : (NumericValue.t, t) Either.t =
  match (b, vs) with
  | _, Second (Undef _, _) | _, Third (_, Undef _) -> Right (Undef outwidth)
  | Bop.Bint_add, Second (SP o, lv) | Bop.Bint_add, Third (lv, SP o) ->
      Right
        (eval_sp_arith o
           (Int64Ext.sext (NumericValue.value_64 lv) (NumericValue.width lv) 8l))
  | Bop.Bint_sub, Second (SP o, rv) ->
      Right
        (eval_sp_arith o
           (Int64.neg
              (Int64Ext.sext (NumericValue.value_64 rv) (NumericValue.width rv)
                 8l)))
  | Bop.Bint_sub, Third (rv, SP o) ->
      Right
        (SP
           {
             o with
             multiplier = Int64.neg o.multiplier;
             offset = Int64.sub (NumericValue.value_64 rv) o.offset;
           })
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
               })
      else Right (Undef outwidth)
  | Bop.Bint_sub, First (v1, v2) ->
      if (match v1 with Undef _ -> false | _ -> true) && v1 = v2 then
        Left (NumericValue.zero outwidth)
      else Right (Undef outwidth)
  | Bop.Bint_xor, First (v1, v2) ->
      if (match v1 with Undef _ -> false | _ -> true) && v1 = v2 then
        Left (NumericValue.zero outwidth)
      else Right (Undef outwidth)
  | Bop.Bint_equal, First (SP o1, SP o2) ->
      if
        (o1.timestamp, o1.func, o1.multiplier, o1.offset)
        = (o2.timestamp, o2.func, o2.multiplier, o2.offset)
      then Left (NumericValue.of_int64 1L 1l)
      else Left (NumericValue.of_int64 0L 1l)
  | Bop.Bint_equal, Second _ | Bop.Bint_equal, Third _ ->
      Left (NumericValue.of_int64 0L 1l)
  | Bop.Bint_notequal, First (SP o1, SP o2) ->
      if (o1.timestamp, o1.func, o1.offset) = (o2.timestamp, o2.func, o2.offset)
      then Left (NumericValue.of_int64 0L 1l)
      else Left (NumericValue.of_int64 1L 1l)
  | Bop.Bint_notequal, Second _ | Bop.Bint_notequal, Third _ ->
      Left (NumericValue.of_int64 1L 1l)
  | _ -> Right (Undef outwidth)

let width (v : t) : Int32.t = match v with Undef width -> width | _ -> 8l
let undefined (width : Int32.t) : t = Undef width
let sp (v : SPVal.t) : t = SP v

let get_sp (v : t) : SPVal.t Option.t =
  match v with SP v -> Some v | _ -> None
