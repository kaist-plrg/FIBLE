open StdlibExt
open Basic
open Basic_collection

let ( let* ) = Result.bind

let eval (b : Bop.t) (lv : NumericValue.t) (rv : NumericValue.t)
    (outwidth : Int32.t) : (NumericValue.t, String.t) Result.t =
  match b with
  | Bpiece ->
      let* fv = Int64Ext.concat lv.value rv.value lv.width rv.width in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Bsubpiece ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width lv.value
           (Int32.min (Int32.sub lv.width (Int64.to_int32 rv.value)) outwidth))
        outwidth
  | Bint_equal ->
      if lv.width <> rv.width then Error "int_equal: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if lv.value = rv.value then 1L else 0L)
          outwidth
  | Bint_notequal ->
      if lv.width <> rv.width then Error "int_notequal: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if lv.value <> rv.value then 1L else 0L)
          outwidth
  | Bint_less ->
      if lv.width <> rv.width then Error "int_less: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if Int64.unsigned_compare lv.value rv.value < 0 then 1L else 0L)
          outwidth
  | Bint_sless ->
      if lv.width <> rv.width then Error "int_sless: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64.compare
               (Int64Ext.sext lv.value lv.width 8l)
               (Int64Ext.sext rv.value rv.width 8l)
             < 0
           then 1L
           else 0L)
          outwidth
  | Bint_lessequal ->
      if lv.width <> rv.width then Error "int_lessequal: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if Int64.unsigned_compare lv.value rv.value <= 0 then 1L else 0L)
          outwidth
  | Bint_slessequal ->
      if lv.width <> rv.width then Error "int_slessequal: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64.compare
               (Int64Ext.sext lv.value lv.width 8l)
               (Int64Ext.sext rv.value rv.width 8l)
             <= 0
           then 1L
           else 0L)
          outwidth
  | Bint_add ->
      if lv.width <> rv.width then Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64.add lv.value rv.value) outwidth)
          outwidth
  | Bint_sub ->
      if lv.width <> rv.width then Error "int_sub: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64.sub lv.value rv.value) outwidth)
          outwidth
  | Bint_carry ->
      if lv.width <> rv.width then Error "int_carry: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if Int64Ext.carry lv.value rv.value lv.width then 1L else 0L)
          outwidth
  | Bint_scarry ->
      if lv.width <> rv.width then Error "int_scarry: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if Int64Ext.scarry lv.value rv.value lv.width then 1L else 0L)
          outwidth
  | Bint_sborrow ->
      if lv.width <> rv.width then Error "int_sborrow: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if Int64Ext.sborrow lv.value rv.value lv.width then 1L else 0L)
          outwidth
  | Bint_xor ->
      if lv.width <> rv.width then Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.logxor lv.value rv.value) outwidth)
          outwidth
  | Bint_and ->
      if lv.width <> rv.width then Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.logand lv.value rv.value) outwidth)
          outwidth
  | Bint_or ->
      if lv.width <> rv.width then Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.logor lv.value rv.value) outwidth)
          outwidth
  | Bint_left ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64Ext.shift_left lv.value (Int64.to_int rv.value))
           outwidth)
        outwidth
  | Bint_right ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64Ext.shift_right_logical lv.value (Int64.to_int rv.value))
           outwidth)
        outwidth
  | Bint_sright ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64Ext.shift_right
              (Int64Ext.sext lv.value lv.width 8l)
              (Int64.to_int rv.value))
           outwidth)
        outwidth
  | Bint_mult ->
      if lv.width <> rv.width then Error "int_mul: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.mul lv.value rv.value) outwidth)
          outwidth
  | Bint_div ->
      if lv.width <> rv.width then Error "int_div: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.unsigned_div lv.value rv.value)
             outwidth)
          outwidth
  | Bint_rem ->
      if lv.width <> rv.width then Error "int_rem: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.unsigned_rem lv.value rv.value)
             outwidth)
          outwidth
  | Bint_sdiv ->
      if lv.width <> rv.width then Error "int_sdiv: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.div lv.value rv.value) outwidth)
          outwidth
  | Bint_srem ->
      if lv.width <> rv.width then Error "int_srem: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.rem lv.value rv.value) outwidth)
          outwidth
  | Bbool_xor ->
      if lv.width <> rv.width then Error "bool_xor: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logxor
                (Int64Ext.logand lv.value 1L)
                (Int64Ext.logand rv.value 1L))
             outwidth)
          outwidth
  | Bbool_and ->
      if lv.width <> rv.width then Error "bool_xor: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logand
                (Int64Ext.logand lv.value 1L)
                (Int64Ext.logand rv.value 1L))
             outwidth)
          outwidth
  | Bbool_or ->
      if lv.width <> rv.width then Error "bool_xor: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logor
                (Int64Ext.logand lv.value 1L)
                (Int64Ext.logand rv.value 1L))
             outwidth)
          outwidth
  | Bfloat_equal ->
      if lv.width <> rv.width then Error "float_equal: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width lv.value lv.width in
        let* rf = Int64Ext.to_float_width rv.value rv.width in
        let rv = if Float.compare lf rf = 0 then 1L else 0L in
        [%log finfo "float" "%f = %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_notequal ->
      if lv.width <> rv.width then Error "float_notequal: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width lv.value lv.width in
        let* rf = Int64Ext.to_float_width rv.value rv.width in
        let rv = if Float.compare lf rf <> 0 then 1L else 0L in
        [%log finfo "float" "%f <> %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_less ->
      if lv.width <> rv.width then Error "float_less: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width lv.value lv.width in
        let* rf = Int64Ext.to_float_width rv.value rv.width in
        let rv = if Float.compare lf rf < 0 then 1L else 0L in
        [%log finfo "float" "%f < %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_lessequal ->
      if lv.width <> rv.width then Error "float_lessequal: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width lv.value lv.width in
        let* rf = Int64Ext.to_float_width rv.value rv.width in
        let rv = if Float.compare lf rf <= 0 then 1L else 0L in
        [%log finfo "float" "%f <= %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_add ->
      if lv.width <> rv.width then Error "float_add: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width lv.value lv.width in
        let* rf = Int64Ext.to_float_width rv.value rv.width in
        let fv = Float.add lf rf in
        [%log finfo "float" "%f + %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_sub ->
      if lv.width <> rv.width then Error "float_sub: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width lv.value lv.width in
        let* rf = Int64Ext.to_float_width rv.value rv.width in
        let fv = Float.sub lf rf in
        [%log finfo "float" "%f - %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_mult ->
      if lv.width <> rv.width then Error "float_mult: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width lv.value lv.width in
        let* rf = Int64Ext.to_float_width rv.value rv.width in
        let fv = Float.mul lf rf in
        [%log finfo "float" "%f * %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_div ->
      if lv.width <> rv.width then Error "float_div: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width lv.value lv.width in
        let* rf = Int64Ext.to_float_width rv.value rv.width in
        let fv = Float.div lf rf in
        [%log finfo "float" "%f / %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
