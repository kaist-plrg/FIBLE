let carry (l : Z.t) (r : Z.t) (width : int32) : bool =
  Z.size (Z.add l r) > Int32.to_int width * 8

let scarry (l : Z.t) (r : Z.t) (width : int32) : bool =
  let msbl = Z.logand (Z.shift_right l ((Int32.to_int width * 8) - 1)) Z.one in
  let msbr = Z.logand (Z.shift_right r ((Int32.to_int width * 8) - 1)) Z.one in
  let msb =
    Z.logand (Z.shift_right (Z.add l r) ((Int32.to_int width * 8) - 1)) Z.one
  in
  (msbl = Z.one && msbr = Z.one && msb = Z.zero)
  || (msbl = Z.zero && msbr = Z.zero && msb = Z.one)

let sborrow (l : Z.t) (r : Z.t) (width : int32) : bool =
  scarry l (Z.extract (Z.neg r) 0 ((width |> Int32.to_int) * 8)) width

let eval (b : Bop.t) (lv : NumericValue.t) (rv : NumericValue.t)
    (outwidth : Int32.t) : (NumericValue.t, String.t) Result.t =
  let* ln64 = NumericValue.value_64 lv in
  let* ln = NumericValue.value_z lv in
  let* ln_s = NumericValue.value_signed_z lv in
  let* rn64 = NumericValue.value_64 rv in
  let* rn = NumericValue.value_z rv in
  let* rn_s = NumericValue.value_signed_z rv in
  match b with
  | Bpiece -> List.append rv lv |> Result.ok
  | Bsubpiece ->
      NumericValue.sublist lv (Z.to_int rn) (Int32.to_int outwidth) |> Result.ok
  | Bint_equal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_equal: different bitwidth"
      else
        NumericValue.of_z (if lv = rv then Z.one else Z.zero) outwidth
        |> Result.ok
  | Bint_notequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_notequal: different bitwidth"
      else
        NumericValue.of_z (if lv <> rv then Z.one else Z.zero) outwidth
        |> Result.ok
  | Bint_less ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_less: different bitwidth"
      else
        NumericValue.of_z
          (if Z.compare ln rn < 0 then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_sless ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sless: different bitwidth"
      else
        NumericValue.of_z
          (if Z.compare ln_s rn_s < 0 then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_lessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_lessequal: different bitwidth"
      else
        NumericValue.of_z
          (if Z.compare ln rn <= 0 then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_slessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_slessequal: different bitwidth"
      else
        NumericValue.of_z
          (if Z.compare ln_s rn_s <= 0 then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_add ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else NumericValue.of_z (Z.add ln rn) outwidth |> Result.ok
  | Bint_sub ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sub: different bitwidth"
      else NumericValue.of_z (Z.sub ln rn) outwidth |> Result.ok
  | Bint_carry ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_carry: different bitwidth"
      else
        NumericValue.of_z
          (if carry ln rn (NumericValue.width lv) then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_scarry ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_scarry: different bitwidth"
      else
        NumericValue.of_z
          (if scarry ln rn (NumericValue.width lv) then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_sborrow ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sborrow: different bitwidth"
      else
        NumericValue.of_z
          (if sborrow ln rn (NumericValue.width lv) then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_xor ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else NumericValue.of_z (Z.logxor ln rn) outwidth |> Result.ok
  | Bint_and ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else NumericValue.of_z (Z.logand ln rn) outwidth |> Result.ok
  | Bint_or ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else NumericValue.of_z (Z.logor ln rn) outwidth |> Result.ok
  | Bint_left ->
      NumericValue.of_z (Z.shift_left ln (Z.to_int rn)) outwidth |> Result.ok
  | Bint_right ->
      NumericValue.of_z (Z.shift_right ln (Z.to_int rn)) outwidth |> Result.ok
  | Bint_sright ->
      NumericValue.of_z (Z.shift_right ln_s (Z.to_int rn)) outwidth |> Result.ok
  | Bint_mult ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_mul: different bitwidth"
      else NumericValue.of_z (Z.mul ln rn) outwidth |> Result.ok
  | Bint_div ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_div: different bitwidth"
      else NumericValue.of_z (Z.div ln rn) outwidth |> Result.ok
  | Bint_rem ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_rem: different bitwidth"
      else NumericValue.of_z (Z.rem ln rn) outwidth |> Result.ok
  | Bint_sdiv ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sdiv: different bitwidth"
      else NumericValue.of_z (Z.div ln_s rn_s) outwidth |> Result.ok
  | Bint_srem ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_srem: different bitwidth"
      else NumericValue.of_z (Z.rem ln_s rn_s) outwidth |> Result.ok
  | Bbool_xor ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_xor: different bitwidth"
      else
        NumericValue.of_z
          (Z.logxor (Z.logand ln Z.one) (Z.logand rn Z.one))
          outwidth
        |> Result.ok
  | Bbool_and ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_xor: different bitwidth"
      else
        NumericValue.of_z
          (Z.logand (Z.logand ln Z.one) (Z.logand rn Z.one))
          outwidth
        |> Result.ok
  | Bbool_or ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_xor: different bitwidth"
      else
        NumericValue.of_z
          (Z.logor (Z.logand ln Z.one) (Z.logand rn Z.one))
          outwidth
        |> Result.ok
  | Bfloat_equal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_equal: different bitwidth"
      else
        let* lf = Int64.to_float_width ln64 (NumericValue.width lv) in
        let* rf = Int64.to_float_width rn64 (NumericValue.width rv) in
        let rv = if Float.compare lf rf = 0 then 1L else 0L in
        [%log finfo "float" "%f = %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_notequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_notequal: different bitwidth"
      else
        let* lf = Int64.to_float_width ln64 (NumericValue.width lv) in
        let* rf = Int64.to_float_width rn64 (NumericValue.width rv) in
        let rv = if Float.compare lf rf <> 0 then 1L else 0L in
        [%log finfo "float" "%f <> %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_less ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_less: different bitwidth"
      else
        let* lf = Int64.to_float_width ln64 (NumericValue.width lv) in
        let* rf = Int64.to_float_width rn64 (NumericValue.width rv) in
        let rv = if Float.compare lf rf < 0 then 1L else 0L in
        [%log finfo "float" "%f < %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_lessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_lessequal: different bitwidth"
      else
        let* lf = Int64.to_float_width ln64 (NumericValue.width lv) in
        let* rf = Int64.to_float_width rn64 (NumericValue.width rv) in
        let rv = if Float.compare lf rf <= 0 then 1L else 0L in
        [%log finfo "float" "%f <= %f = %Lx" lf rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_add ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_add: different bitwidth"
      else
        let* lf = Int64.to_float_width ln64 (NumericValue.width lv) in
        let* rf = Int64.to_float_width rn64 (NumericValue.width rv) in
        let fv = Float.add lf rf in
        [%log finfo "float" "%f + %f = %f" lf rf fv];
        let* fv = Int64.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_sub ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_sub: different bitwidth"
      else
        let* lf = Int64.to_float_width ln64 (NumericValue.width lv) in
        let* rf = Int64.to_float_width rn64 (NumericValue.width rv) in
        let fv = Float.sub lf rf in
        [%log finfo "float" "%f - %f = %f" lf rf fv];
        let* fv = Int64.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_mult ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_mult: different bitwidth"
      else
        let* lf = Int64.to_float_width ln64 (NumericValue.width lv) in
        let* rf = Int64.to_float_width rn64 (NumericValue.width rv) in
        let fv = Float.mul lf rf in
        [%log finfo "float" "%f * %f = %f" lf rf fv];
        let* fv = Int64.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
  | Bfloat_div ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_div: different bitwidth"
      else
        let* lf = Int64.to_float_width ln64 (NumericValue.width lv) in
        let* rf = Int64.to_float_width rn64 (NumericValue.width rv) in
        let fv = Float.div lf rf in
        [%log finfo "float" "%f / %f = %f" lf rf fv];
        let* fv = Int64.of_float_width fv outwidth in
        NumericValue.of_int64_safe fv outwidth
