open StdlibExt
open Basic
open Common_language

let ( let* ) = Result.bind

type t =
  | LocalP of LPVal.t
  | ParamP of PPVal.t
  | SP of SPVal.t
  | Undef of Int32.t

let pp fmt = function
  | LocalP lp -> LPVal.pp fmt lp
  | ParamP pp -> PPVal.pp fmt pp
  | SP sp -> SPVal.pp fmt sp
  | Undef i -> Format.fprintf fmt "undef_%ld" i

let eval_uop (u : Uop.t) (v : t) (outwidth : Int32.t) :
    (NumericValue.t, t) Either.t =
  Right (Undef outwidth)

let eval_sp_arith (o : SPVal.t) (v : Int64.t) : t =
  if v > 0L then ParamP { timestamp = o.timestamp; func = o.func; offset = v }
  else if v < 0L then
    LocalP { timestamp = o.timestamp; func = o.func; offset = v }
  else SP o

let eval_bop (b : Bop.t)
    (vs : (t * t, t * NumericValue.t, NumericValue.t * t) Either3.t)
    (outwidth : Int32.t) : (NumericValue.t, t) Either.t =
  match (b, vs) with
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
  | Bop.Bint_add, Second (LocalP o, lv) | Bop.Bint_add, Third (lv, LocalP o) ->
      Right
        (LocalP
           {
             o with
             offset =
               Int64.add o.offset
                 (Int64Ext.sext (NumericValue.value_64 lv)
                    (NumericValue.width lv) 8l);
           })
  | Bop.Bint_sub, Second (LocalP o, rv) ->
      Right
        (LocalP
           {
             o with
             offset =
               Int64.sub o.offset
                 (Int64Ext.sext (NumericValue.value_64 rv)
                    (NumericValue.width rv) 8l);
           })
  | Bop.Bint_add, Second (ParamP o, lv) | Bop.Bint_add, Third (lv, ParamP o) ->
      Right
        (ParamP
           {
             o with
             offset =
               Int64.add o.offset
                 (Int64Ext.sext (NumericValue.value_64 lv)
                    (NumericValue.width lv) 8l);
           })
  | Bop.Bint_sub, Second (ParamP o, rv) ->
      Right
        (ParamP
           {
             o with
             offset =
               Int64.sub o.offset
                 (Int64Ext.sext (NumericValue.value_64 rv)
                    (NumericValue.width rv) 8l);
           })
  | Bop.Bint_sub, First (LocalP o1, LocalP o2) ->
      if (o1.timestamp, o1.func) = (o2.timestamp, o2.func) then
        Left (NumericValue.of_int64 (Int64.sub o1.offset o2.offset) outwidth)
      else Right (Undef outwidth)
  | Bop.Bint_sub, First (ParamP o1, ParamP o2) ->
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
  | Bop.Bint_equal, First (LocalP o1, LocalP o2) -> Right (Undef outwidth)
  | Bop.Bint_equal, Second _ | Bop.Bint_equal, Third _ ->
      Left (NumericValue.of_int64 0L 1l)
  | Bop.Bint_notequal, First (LocalP o1, LocalP o2) -> Right (Undef outwidth)
  | Bop.Bint_notequal, Second _ | Bop.Bint_notequal, Third _ ->
      Left (NumericValue.of_int64 1L 1l)
  | _ -> Right (Undef outwidth)

let refine_width (v : t) (width : int32) : t =
  match v with
  | LocalP v -> LocalP v
  | ParamP v -> ParamP v
  | SP v -> SP v
  | Undef _ -> Undef width

let width_of (v : t) : Int32.t = match v with Undef width -> width | _ -> 8l
let undefined (width : Int32.t) : t = Undef width
