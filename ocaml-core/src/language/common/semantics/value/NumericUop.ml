open StdlibExt
open Notation

let eval (u : Uop.t) (v : NumericValue.t) (outwidth : Int32.t) :
    (NumericValue.t, String.t) Result.t =
  let* n = NumericValue.value_64 v in
  match u with
  | Upopcount ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width (Int64Ext.bitcount n) outwidth)
        outwidth
  | Ulzcount ->
      NumericValue.of_int64_safe
        (Int64Ext.sub
           (Int64.of_int32 (NumericValue.width v))
           (Int64Ext.bitwidth n))
        outwidth
  | Uint_zext ->
      NumericValue.of_int64_safe
        (Int64Ext.zext n (NumericValue.width v) outwidth)
        outwidth
  | Uint_sext ->
      NumericValue.of_int64_safe
        (Int64Ext.sext n (NumericValue.width v) outwidth)
        outwidth
  | Uint_2comp ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width (Int64Ext.neg n) outwidth)
        outwidth
  | Uint_negate ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width (Int64Ext.lognot n) outwidth)
        outwidth
  | Ubool_negate ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width (if Int64Ext.equal n 0L then 1L else 0L) outwidth)
        outwidth
  | Ufloat_neg ->
      let* fv = Int64Ext.float_neg n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_abs ->
      let* fv = Int64Ext.float_abs n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_sqrt ->
      let* fv = Int64Ext.float_sqrt n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_ceil ->
      let* fv = Int64Ext.float_ceil n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_floor ->
      let* fv = Int64Ext.float_floor n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_round ->
      let* fv = Int64Ext.float_round n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_nan ->
      let* fv = Int64Ext.float_round n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Uint2float ->
      let* fv = Int64Ext.int2float n (NumericValue.width v) outwidth in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat2float ->
      let* fv = Int64Ext.float2float n (NumericValue.width v) outwidth in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Utrunc ->
      let* fv = Int64Ext.trunc n (NumericValue.width v) outwidth in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
