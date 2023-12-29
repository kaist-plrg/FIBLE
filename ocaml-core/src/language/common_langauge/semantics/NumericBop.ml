open StdlibExt
open Basic
open Basic_collection

let ( let* ) = Result.bind

let eval (b : Bop.t) (lv : NumericValue.t) (rv : NumericValue.t)
    (outwidth : Int32.t) : (NumericValue.t, String.t) Result.t =
  match b with
  | Bpiece ->
      let* fv =
        Int64Ext.concat (NumericValue.value_64 lv) (NumericValue.value_64 rv)
          (NumericValue.width lv) (NumericValue.width rv)
      in
      NumericValue.of_int64_safe (Int64Ext.cut_width fv outwidth) outwidth
  | Bsubpiece ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64.shift_right_logical (NumericValue.value_64 lv)
              (Int64.to_int (NumericValue.value_64 rv) * 8))
           outwidth)
        outwidth
  | Bint_equal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_equal: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if NumericValue.value_64 lv = NumericValue.value_64 rv then 1L
           else 0L)
          outwidth
  | Bint_notequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_notequal: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if NumericValue.value_64 lv <> NumericValue.value_64 rv then 1L
           else 0L)
          outwidth
  | Bint_less ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_less: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64.unsigned_compare (NumericValue.value_64 lv)
               (NumericValue.value_64 rv)
             < 0
           then 1L
           else 0L)
          outwidth
  | Bint_sless ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sless: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64.compare
               (Int64Ext.sext (NumericValue.value_64 lv) (NumericValue.width lv)
                  8l)
               (Int64Ext.sext (NumericValue.value_64 rv) (NumericValue.width rv)
                  8l)
             < 0
           then 1L
           else 0L)
          outwidth
  | Bint_lessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_lessequal: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64.unsigned_compare (NumericValue.value_64 lv)
               (NumericValue.value_64 rv)
             <= 0
           then 1L
           else 0L)
          outwidth
  | Bint_slessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_slessequal: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64.compare
               (Int64Ext.sext (NumericValue.value_64 lv) (NumericValue.width lv)
                  8l)
               (Int64Ext.sext (NumericValue.value_64 rv) (NumericValue.width rv)
                  8l)
             <= 0
           then 1L
           else 0L)
          outwidth
  | Bint_add ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64.add (NumericValue.value_64 lv) (NumericValue.value_64 rv))
             outwidth)
          outwidth
  | Bint_sub ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sub: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64.sub (NumericValue.value_64 lv) (NumericValue.value_64 rv))
             outwidth)
          outwidth
  | Bint_carry ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_carry: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64Ext.carry (NumericValue.value_64 lv)
               (NumericValue.value_64 rv) (NumericValue.width lv)
           then 1L
           else 0L)
          outwidth
  | Bint_scarry ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_scarry: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64Ext.scarry (NumericValue.value_64 lv)
               (NumericValue.value_64 rv) (NumericValue.width lv)
           then 1L
           else 0L)
          outwidth
  | Bint_sborrow ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sborrow: different bitwidth"
      else
        NumericValue.of_int64_safe
          (if
             Int64Ext.sborrow (NumericValue.value_64 lv)
               (NumericValue.value_64 rv) (NumericValue.width lv)
           then 1L
           else 0L)
          outwidth
  | Bint_xor ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logxor (NumericValue.value_64 lv)
                (NumericValue.value_64 rv))
             outwidth)
          outwidth
  | Bint_and ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logand (NumericValue.value_64 lv)
                (NumericValue.value_64 rv))
             outwidth)
          outwidth
  | Bint_or ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logor (NumericValue.value_64 lv)
                (NumericValue.value_64 rv))
             outwidth)
          outwidth
  | Bint_left ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64Ext.shift_left (NumericValue.value_64 lv)
              (Int64.to_int (NumericValue.value_64 rv)))
           outwidth)
        outwidth
  | Bint_right ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64Ext.shift_right_logical (NumericValue.value_64 lv)
              (Int64.to_int (NumericValue.value_64 rv)))
           outwidth)
        outwidth
  | Bint_sright ->
      NumericValue.of_int64_safe
        (Int64Ext.cut_width
           (Int64Ext.shift_right
              (Int64Ext.sext (NumericValue.value_64 lv) (NumericValue.width lv)
                 8l)
              (Int64.to_int (NumericValue.value_64 rv)))
           outwidth)
        outwidth
  | Bint_mult ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_mul: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.mul (NumericValue.value_64 lv) (NumericValue.value_64 rv))
             outwidth)
          outwidth
  | Bint_div ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_div: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.unsigned_div (NumericValue.value_64 lv)
                (NumericValue.value_64 rv))
             outwidth)
          outwidth
  | Bint_rem ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_rem: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.unsigned_rem (NumericValue.value_64 lv)
                (NumericValue.value_64 rv))
             outwidth)
          outwidth
  | Bint_sdiv ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sdiv: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.div (NumericValue.value_64 lv) (NumericValue.value_64 rv))
             outwidth)
          outwidth
  | Bint_srem ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_srem: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.rem (NumericValue.value_64 lv) (NumericValue.value_64 rv))
             outwidth)
          outwidth
  | Bbool_xor ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_xor: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logxor
                (Int64Ext.logand (NumericValue.value_64 lv) 1L)
                (Int64Ext.logand (NumericValue.value_64 rv) 1L))
             outwidth)
          outwidth
  | Bbool_and ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_xor: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logand
                (Int64Ext.logand (NumericValue.value_64 lv) 1L)
                (Int64Ext.logand (NumericValue.value_64 rv) 1L))
             outwidth)
          outwidth
  | Bbool_or ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_xor: different bitwidth"
      else
        NumericValue.of_int64_safe
          (Int64Ext.cut_width
             (Int64Ext.logor
                (Int64Ext.logand (NumericValue.value_64 lv) 1L)
                (Int64Ext.logand (NumericValue.value_64 rv) 1L))
             outwidth)
          outwidth
  | Bfloat_equal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_equal: different bitwidth"
      else
        let* lf =
          Int64Ext.to_float_width (NumericValue.value_64 lv)
            (NumericValue.width lv)
        in
        let* rf =
          Int64Ext.to_float_width (NumericValue.value_64 rv)
            (NumericValue.width rv)
        in
        let rv = if Float.compare lf rf = 0 then 1L else 0L in
        [%log finfo "float" "%f = %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_notequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_notequal: different bitwidth"
      else
        let* lf =
          Int64Ext.to_float_width (NumericValue.value_64 lv)
            (NumericValue.width lv)
        in
        let* rf =
          Int64Ext.to_float_width (NumericValue.value_64 rv)
            (NumericValue.width rv)
        in
        let rv = if Float.compare lf rf <> 0 then 1L else 0L in
        [%log finfo "float" "%f <> %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_less ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_less: different bitwidth"
      else
        let* lf =
          Int64Ext.to_float_width (NumericValue.value_64 lv)
            (NumericValue.width lv)
        in
        let* rf =
          Int64Ext.to_float_width (NumericValue.value_64 rv)
            (NumericValue.width rv)
        in
        let rv = if Float.compare lf rf < 0 then 1L else 0L in
        [%log finfo "float" "%f < %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_lessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_lessequal: different bitwidth"
      else
        let* lf =
          Int64Ext.to_float_width (NumericValue.value_64 lv)
            (NumericValue.width lv)
        in
        let* rf =
          Int64Ext.to_float_width (NumericValue.value_64 rv)
            (NumericValue.width rv)
        in
        let rv = if Float.compare lf rf <= 0 then 1L else 0L in
        [%log finfo "float" "%f <= %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_add ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_add: different bitwidth"
      else
        let* lf =
          Int64Ext.to_float_width (NumericValue.value_64 lv)
            (NumericValue.width lv)
        in
        let* rf =
          Int64Ext.to_float_width (NumericValue.value_64 rv)
            (NumericValue.width rv)
        in
        let fv = Float.add lf rf in
        [%log finfo "float" "%f + %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_sub ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_sub: different bitwidth"
      else
        let* lf =
          Int64Ext.to_float_width (NumericValue.value_64 lv)
            (NumericValue.width lv)
        in
        let* rf =
          Int64Ext.to_float_width (NumericValue.value_64 rv)
            (NumericValue.width rv)
        in
        let fv = Float.sub lf rf in
        [%log finfo "float" "%f - %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_mult ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_mult: different bitwidth"
      else
        let* lf =
          Int64Ext.to_float_width (NumericValue.value_64 lv)
            (NumericValue.width lv)
        in
        let* rf =
          Int64Ext.to_float_width (NumericValue.value_64 rv)
            (NumericValue.width rv)
        in
        let fv = Float.mul lf rf in
        [%log finfo "float" "%f * %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_div ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_div: different bitwidth"
      else
        let* lf =
          Int64Ext.to_float_width (NumericValue.value_64 lv)
            (NumericValue.width lv)
        in
        let* rf =
          Int64Ext.to_float_width (NumericValue.value_64 rv)
            (NumericValue.width rv)
        in
        let fv = Float.div lf rf in
        [%log finfo "float" "%f / %f = %f" lf rf fv];
        let* fv = Int64Ext.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
