open StdlibExt
open Basic
open Common_language

let ( let* ) = Result.bind

type t = Num of NumericValue.t | SP of SPVal.t | Undef of Int32.t

let zero w = Num (NumericValue.zero w)

let pp fmt = function
  | Num n -> NumericValue.pp fmt n
  | SP sp -> SPVal.pp fmt sp
  | Undef i -> Format.fprintf fmt "undef_%ld" i

let try_addr (v : t) : (Addr.t, String.t) Result.t =
  match v with
  | Num n -> Ok (NumericValue.to_addr n)
  | SP sp -> Error (Format.asprintf "try_addr: sp_%a" SPVal.pp sp)
  | Undef i -> Error (Format.sprintf "try_addr: undef_%ld" i)

let try_loc (v : t) : (Loc.t, String.t) Result.t =
  match v with
  | Num n -> Ok (NumericValue.to_loc n)
  | SP sp -> Error (Format.asprintf "try_loc: sp_%a" SPVal.pp sp)
  | Undef i -> Error (Format.sprintf "try_loc: undef_%ld" i)

let try_isZero (v : t) : (Bool.t, String.t) Result.t =
  match v with
  | Num n -> Ok (NumericValue.isZero n)
  | SP sp -> Error (Format.asprintf "try_isZero: sp_%a" SPVal.pp sp)
  | Undef i -> Error (Format.sprintf "try_isZero: undef_%ld" i)

let eval_uop (u : Uop.t) (v : t) (outwidth : Int32.t) : (t, String.t) Result.t =
  match v with
  | SP _ -> Ok (Undef outwidth)
  | Num vn ->
      let* vn' = NumericUop.eval u vn outwidth in
      Ok (Num vn')
  | Undef _ -> Ok (Undef outwidth)

let eval_bop (b : Bop.t) (lv : t) (rv : t) (outwidth : Int32.t) :
    (t, String.t) Result.t =
  match (b, lv, rv) with
  | _, Num lv, Num rv ->
      let* vn' = NumericBop.eval b lv rv outwidth in
      Ok (Num vn')
  | Bop.Bint_add, SP o, Num rv ->
      Ok
        (SP
           {
             o with
             offset = Int64.add o.offset (Int64Ext.sext rv.value rv.width 8l);
           })
  | Bop.Bint_add, Num lv, SP o ->
      Ok
        (SP
           {
             o with
             offset = Int64.add o.offset (Int64Ext.sext lv.value lv.width 8l);
           })
  | Bop.Bint_sub, SP o, Num rv ->
      Ok
        (SP
           {
             o with
             offset = Int64.sub o.offset (Int64Ext.sext rv.value rv.width 8l);
           })
  | Bop.Bint_sub, SP o1, SP o2 ->
    if (o1.timestamp, o1.func) = (o2.timestamp, o2.func) then Ok (Num (NumericValue.of_int64 (Int64.sub o1.offset o2.offset) outwidth)) else Ok (Undef outwidth)

  | _ -> Ok (Undef outwidth)
