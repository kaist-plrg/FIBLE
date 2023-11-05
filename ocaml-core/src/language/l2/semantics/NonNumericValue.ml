open StdlibExt
open Basic
open Common_language

let ( let* ) = Result.bind

type t = SP of SPVal.t | Undef of Int32.t

let pp fmt = function
  | SP sp -> SPVal.pp fmt sp
  | Undef i -> Format.fprintf fmt "undef_%ld" i

let eval_uop (u : Uop.t) (v : t) (outwidth : Int32.t) :
    (NumericValue.t, t) Either.t =
  Right (Undef outwidth)

let eval_bop (b : Bop.t)
    (vs : (t * t, t * NumericValue.t, NumericValue.t * t) Either3.t)
    (outwidth : Int32.t) : (NumericValue.t, t) Either.t =
  match (b, vs) with
  | Bop.Bint_add, Second (SP o, rv) ->
      Right
        (SP
           {
             o with
             offset = Int64.add o.offset (Int64Ext.sext rv.value rv.width 8l);
           })
  | Bop.Bint_add, Third (lv, SP o) ->
      Right
        (SP
           {
             o with
             offset = Int64.add o.offset (Int64Ext.sext lv.value lv.width 8l);
           })
  | Bop.Bint_sub, Second (SP o, rv) ->
      Right
        (SP
           {
             o with
             offset = Int64.sub o.offset (Int64Ext.sext rv.value rv.width 8l);
           })
  | Bop.Bint_sub, First (SP o1, SP o2) ->
      if (o1.timestamp, o1.func) = (o2.timestamp, o2.func) then
        Left (NumericValue.of_int64 (Int64.sub o1.offset o2.offset) outwidth)
      else Right (Undef outwidth)
  | Bop.Bint_sub, First (v1, v2) ->
      if (match v1 with Undef _ -> false | _ -> true) && v1 = v2 then
        Left (NumericValue.zero outwidth)
      else Right (Undef outwidth)
  | Bop.Bint_xor, First (v1, v2) ->
      if (match v1 with Undef _ -> false | _ -> true) && v1 = v2 then
        Left (NumericValue.zero outwidth)
      else Right (Undef outwidth)
  | _ -> Right (Undef outwidth)

let refine_width (v : t) (width : int32) : t =
  match v with SP v -> SP v | Undef _ -> Undef width

let width_of (v : t) : Int32.t =
  match v with SP _ -> 8l | Undef width -> width

let undefined width = Undef width
