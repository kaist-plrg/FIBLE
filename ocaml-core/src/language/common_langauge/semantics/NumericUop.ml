open StdlibExt
open Notation
open Basic
open Basic_collection

let eval (u : Uop.t) (v : NumericValue.t) (outwidth : Int32.t) :
    (NumericValue.t, String.t) Result.t =
  match u with
  | Upopcount ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64Ext.bitcount (NumericValue.value_64 v))
           outwidth)
        outwidth
  | Ulzcount ->
      NumericValue.of_int64_safe
        (Int64Ext.sub
           (Int64.of_int32 (NumericValue.width v))
           (Int64Ext.bitwidth (NumericValue.value_64 v)))
        outwidth
  | Uint_zext ->
      NumericValue.of_int64_safe
        (Int64Ext.zext (NumericValue.value_64 v) (NumericValue.width v) outwidth)
        outwidth
  | Uint_sext ->
      NumericValue.of_int64_safe
        (Int64Ext.sext (NumericValue.value_64 v) (NumericValue.width v) outwidth)
        outwidth
  | Uint_2comp ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width (Int64Ext.neg (NumericValue.value_64 v)) outwidth)
        outwidth
  | Uint_negate ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64Ext.lognot (NumericValue.value_64 v))
           outwidth)
        outwidth
  | Ubool_negate ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (if Int64Ext.equal (NumericValue.value_64 v) 0L then 1L else 0L)
           outwidth)
        outwidth
  | Ufloat_neg ->
      let* fv =
        Int64Ext.float_neg (NumericValue.value_64 v) (NumericValue.width v)
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_abs ->
      let* fv =
        Int64Ext.float_abs (NumericValue.value_64 v) (NumericValue.width v)
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_sqrt ->
      let* fv =
        Int64Ext.float_sqrt (NumericValue.value_64 v) (NumericValue.width v)
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_ceil ->
      let* fv =
        Int64Ext.float_ceil (NumericValue.value_64 v) (NumericValue.width v)
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_floor ->
      let* fv =
        Int64Ext.float_floor (NumericValue.value_64 v) (NumericValue.width v)
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_round ->
      let* fv =
        Int64Ext.float_round (NumericValue.value_64 v) (NumericValue.width v)
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_nan ->
      let* fv =
        Int64Ext.float_round (NumericValue.value_64 v) (NumericValue.width v)
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Uint2float ->
      let* fv =
        Int64Ext.int2float (NumericValue.value_64 v) (NumericValue.width v)
          outwidth
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat2float ->
      let* fv =
        Int64Ext.float2float (NumericValue.value_64 v) (NumericValue.width v)
          outwidth
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Utrunc ->
      let* fv =
        Int64Ext.trunc (NumericValue.value_64 v) (NumericValue.width v) outwidth
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
