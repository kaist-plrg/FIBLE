open StdlibExt
open Basic
open Basic_collection

let ( let* ) = Result.bind

let eval (u : Uop.t) (v : NumericValue.t) (outwidth : Int32.t) :
    (NumericValue.t, String.t) Result.t =
  match u with
  | Upopcount ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width (Int64Ext.bitcount v.value) outwidth)
        outwidth
  | Ulzcount ->
      NumericValue.of_int64_safe
        (Int64Ext.sub (Int64.of_int32 v.width) (Int64Ext.bitwidth v.value))
        outwidth
  | Uint_zext ->
      NumericValue.of_int64_safe
        (Int64Ext.zext v.value v.width outwidth)
        outwidth
  | Uint_sext ->
      NumericValue.of_int64_safe
        (Int64Ext.sext v.value v.width outwidth)
        outwidth
  | Uint_2comp ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width (Int64Ext.neg v.value) outwidth)
        outwidth
  | Uint_negate ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width (Int64Ext.lognot v.value) outwidth)
        outwidth
  | Ubool_negate ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (if Int64Ext.equal v.value 0L then 1L else 0L)
           outwidth)
        outwidth
  | Ufloat_neg ->
      let* fv = Int64Ext.float_neg v.value v.width in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_abs ->
      let* fv = Int64Ext.float_abs v.value v.width in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_sqrt ->
      let* fv = Int64Ext.float_sqrt v.value v.width in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_ceil ->
      let* fv = Int64Ext.float_ceil v.value v.width in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_floor ->
      let* fv = Int64Ext.float_floor v.value v.width in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_round ->
      let* fv = Int64Ext.float_round v.value v.width in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat_nan ->
      let* fv = Int64Ext.float_round v.value v.width in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Uint2float ->
      let* fv = Int64Ext.int2float v.value v.width outwidth in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Ufloat2float ->
      let* fv = Int64Ext.int2float v.value v.width outwidth in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Utrunc ->
      let* fv = Int64Ext.trunc v.value v.width outwidth in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
