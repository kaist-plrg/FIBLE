open StdlibExt
open Notation

let eval (b : Bop.t) (lv : NumericValue.t) (rv : NumericValue.t)
    (outwidth : Int32.t) : (NumericValue.t, String.t) Result.t =
  let* ln = NumericValue.value_64 lv in
  let* rn = NumericValue.value_64 rv in
  match b with
  | Bpiece ->
      let* fv =
        Int64Ext.concat ln rn (NumericValue.width lv) (NumericValue.width rv)
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Bsubpiece ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64.shift_right_logical ln (Int64.to_int rn * 8))
           outwidth)
        outwidth
  | Bint_equal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_equal: different bitwidth"
      else NumericValue.of_int64_safe (if ln = rn then 1L else 0L) outwidth
  | Bint_notequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_notequal: different bitwidth"
      else NumericValue.of_int64_safe (if ln <> rn then 1L else 0L) outwidth
  | Bint_less ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_less: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if Int64.unsigned_compare ln rn < 0 then 1L else 0L)
          outwidth
  | Bint_sless ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sless: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64.compare
               (Int64Ext.sext ln (NumericValue.width lv) 8l)
               (Int64Ext.sext rn (NumericValue.width rv) 8l)
             < 0
           then 1L
           else 0L)
          outwidth
  | Bint_lessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_lessequal: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if Int64.unsigned_compare ln rn <= 0 then 1L else 0L)
          outwidth
  | Bint_slessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_slessequal: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64.compare
               (Int64Ext.sext ln (NumericValue.width lv) 8l)
               (Int64Ext.sext rn (NumericValue.width rv) 8l)
             <= 0
           then 1L
           else 0L)
          outwidth
  | Bint_add ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64.add ln rn) outwidth)
          outwidth
  | Bint_sub ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sub: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64.sub ln rn) outwidth)
          outwidth
  | Bint_carry ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_carry: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if Int64Ext.carry ln rn (NumericValue.width lv) then 1L else 0L)
          outwidth
  | Bint_scarry ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_scarry: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if Int64Ext.scarry ln rn (NumericValue.width lv) then 1L else 0L)
          outwidth
  | Bint_sborrow ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sborrow: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if Int64Ext.sborrow ln rn (NumericValue.width lv) then 1L else 0L)
          outwidth
  | Bint_xor ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.logxor ln rn) outwidth)
          outwidth
  | Bint_and ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.logand ln rn) outwidth)
          outwidth
  | Bint_or ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.logor ln rn) outwidth)
          outwidth
  | Bint_left ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width (Int64Ext.shift_left ln (Int64.to_int rn)) outwidth)
        outwidth
  | Bint_right ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64Ext.shift_right_logical ln (Int64.to_int rn))
           outwidth)
        outwidth
  | Bint_sright ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64Ext.shift_right
              (Int64Ext.sext ln (NumericValue.width lv) 8l)
              (Int64.to_int rn))
           outwidth)
        outwidth
  | Bint_mult ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_mul: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.mul ln rn) outwidth)
          outwidth
  | Bint_div ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_div: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.unsigned_div ln rn) outwidth)
          outwidth
  | Bint_rem ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_rem: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.unsigned_rem ln rn) outwidth)
          outwidth
  | Bint_sdiv ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sdiv: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.div ln rn) outwidth)
          outwidth
  | Bint_srem ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_srem: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width (Int64Ext.rem ln rn) outwidth)
          outwidth
  | Bbool_xor ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_xor: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logxor (Int64Ext.logand ln 1L) (Int64Ext.logand rn 1L))
             outwidth)
          outwidth
  | Bbool_and ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_xor: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logand (Int64Ext.logand ln 1L) (Int64Ext.logand rn 1L))
             outwidth)
          outwidth
  | Bbool_or ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_xor: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logor (Int64Ext.logand ln 1L) (Int64Ext.logand rn 1L))
             outwidth)
          outwidth
  | Bfloat_equal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_equal: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width ln (NumericValue.width lv) in
        let* rf = Int64Ext.to_float_width rn (NumericValue.width rv) in
        let rv = if Float.compare lf rf = 0 then 1L else 0L in
        [%log finfo "float" "%f = %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_notequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_notequal: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width ln (NumericValue.width lv) in
        let* rf = Int64Ext.to_float_width rn (NumericValue.width rv) in
        let rv = if Float.compare lf rf <> 0 then 1L else 0L in
        [%log finfo "float" "%f <> %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_less ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_less: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width ln (NumericValue.width lv) in
        let* rf = Int64Ext.to_float_width rn (NumericValue.width rv) in
        let rv = if Float.compare lf rf < 0 then 1L else 0L in
        [%log finfo "float" "%f < %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_lessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_lessequal: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width ln (NumericValue.width lv) in
        let* rf = Int64Ext.to_float_width rn (NumericValue.width rv) in
        let rv = if Float.compare lf rf <= 0 then 1L else 0L in
        [%log finfo "float" "%f <= %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_add ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_add: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width ln (NumericValue.width lv) in
        let* rf = Int64Ext.to_float_width rn (NumericValue.width rv) in
        let fv = Float.add lf rf in
        [%log finfo "float" "%f + %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_sub ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_sub: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width ln (NumericValue.width lv) in
        let* rf = Int64Ext.to_float_width rn (NumericValue.width rv) in
        let fv = Float.sub lf rf in
        [%log finfo "float" "%f - %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_mult ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_mult: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width ln (NumericValue.width lv) in
        let* rf = Int64Ext.to_float_width rn (NumericValue.width rv) in
        let fv = Float.mul lf rf in
        [%log finfo "float" "%f * %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_div ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_div: different bitwidth"
      else
        let* lf = Int64Ext.to_float_width ln (NumericValue.width lv) in
        let* rf = Int64Ext.to_float_width rn (NumericValue.width rv) in
        let fv = Float.div lf rf in
        [%log finfo "float" "%f / %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
