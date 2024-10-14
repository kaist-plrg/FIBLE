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
  match b with
  | Bpiece -> List.append rv lv |> Result.ok
  | Bsubpiece ->
      let* rn = NumericValue.value_z rv in
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
        let* ln = NumericValue.value_z lv in
        let* rn = NumericValue.value_z rv in
        NumericValue.of_z
          (if Z.compare ln rn < 0 then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_sless ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sless: different bitwidth"
      else
        let* ln_s = NumericValue.value_signed_z lv in
        let* rn_s = NumericValue.value_signed_z rv in
        NumericValue.of_z
          (if Z.compare ln_s rn_s < 0 then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_lessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_lessequal: different bitwidth"
      else
        let* ln = NumericValue.value_z lv in
        let* rn = NumericValue.value_z rv in
        NumericValue.of_z
          (if Z.compare ln rn <= 0 then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_slessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_slessequal: different bitwidth"
      else
        let* ln_s = NumericValue.value_signed_z lv in
        let* rn_s = NumericValue.value_signed_z rv in
        NumericValue.of_z
          (if Z.compare ln_s rn_s <= 0 then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_add ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_add: different bitwidth"
      else
        let* ln = NumericValue.value_z lv in
        let* rn = NumericValue.value_z rv in
        NumericValue.of_z (Z.add ln rn) outwidth |> Result.ok
  | Bint_sub ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sub: different bitwidth"
      else
        let* ln = NumericValue.value_z lv in
        let* rn = NumericValue.value_z rv in
        NumericValue.of_z (Z.sub ln rn) outwidth |> Result.ok
  | Bint_carry ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_carry: different bitwidth"
      else
        let* ln = NumericValue.value_z lv in
        let* rn = NumericValue.value_z rv in
        NumericValue.of_z
          (if carry ln rn (NumericValue.width lv) then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_scarry ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_scarry: different bitwidth"
      else
        let* ln = NumericValue.value_z lv in
        let* rn = NumericValue.value_z rv in
        NumericValue.of_z
          (if scarry ln rn (NumericValue.width lv) then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_sborrow ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sborrow: different bitwidth"
      else
        let* ln = NumericValue.value_z lv in
        let* rn = NumericValue.value_z rv in
        NumericValue.of_z
          (if sborrow ln rn (NumericValue.width lv) then Z.one else Z.zero)
          outwidth
        |> Result.ok
  | Bint_xor ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_xor: different bitwidth"
      else NumericValue.bitwise_xor lv rv |> Result.ok
  | Bint_and ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_and: different bitwidth"
      else NumericValue.bitwise_and lv rv |> Result.ok
  | Bint_or ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_or: different bitwidth"
      else NumericValue.bitwise_or lv rv |> Result.ok
  | Bint_left ->
      let* ln = NumericValue.value_z lv in
      let* rn = NumericValue.value_z rv in

      NumericValue.of_z (Z.shift_left ln (Z.to_int rn)) outwidth |> Result.ok
  | Bint_right ->
      let* ln = NumericValue.value_z lv in

      let* rn = NumericValue.value_z rv in

      NumericValue.of_z (Z.shift_right ln (Z.to_int rn)) outwidth |> Result.ok
  | Bint_sright ->
      let* ln_s = NumericValue.value_signed_z lv in

      let* rn = NumericValue.value_z rv in

      NumericValue.of_z (Z.shift_right ln_s (Z.to_int rn)) outwidth |> Result.ok
  | Bint_mult ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_mul: different bitwidth"
      else
        let* ln = NumericValue.value_z lv in
        let* rn = NumericValue.value_z rv in
        NumericValue.of_z (Z.mul ln rn) outwidth |> Result.ok
  | Bint_div ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_div: different bitwidth"
      else
        let* ln = NumericValue.value_z lv in
        let* rn = NumericValue.value_z rv in
        if Z.equal rn Z.zero then Error "int_div: divide by zero"
        else NumericValue.of_z (Z.div ln rn) outwidth |> Result.ok
  | Bint_rem ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_rem: different bitwidth"
      else
        let* ln = NumericValue.value_z lv in
        let* rn = NumericValue.value_z rv in
        if Z.equal rn Z.zero then Error "int_rem: divide by zero"
        else NumericValue.of_z (Z.rem ln rn) outwidth |> Result.ok
  | Bint_sdiv ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_sdiv: different bitwidth"
      else
        let* ln_s = NumericValue.value_signed_z lv in
        let* rn_s = NumericValue.value_signed_z rv in
        if Z.equal rn_s Z.zero then Error "int_sdiv: divide by zero"
        else NumericValue.of_z (Z.div ln_s rn_s) outwidth |> Result.ok
  | Bint_srem ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "int_srem: different bitwidth"
      else
        let* ln_s = NumericValue.value_signed_z lv in
        let* rn_s = NumericValue.value_signed_z rv in
        if Z.equal rn_s Z.zero then Error "int_srem: divide by zero"
        else NumericValue.of_z (Z.rem ln_s rn_s) outwidth |> Result.ok
  | Bbool_xor ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_xor: different bitwidth"
      else List.take 1 (NumericValue.bitwise_xor lv rv) |> Result.ok
  | Bbool_and ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_and: different bitwidth"
      else List.take 1 (NumericValue.bitwise_and lv rv) |> Result.ok
  | Bbool_or ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "bool_or: different bitwidth"
      else List.take 1 (NumericValue.bitwise_or lv rv) |> Result.ok
  | Bfloat_equal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_equal: different bitwidth"
      else
        let* lf = NumericValue.value_float lv in
        let* rf = NumericValue.value_float rv in
        let rv = if Float.compare lf rf = 0 then 1L else 0L in
        [%log finfo "float" "%a == %a = %Lx" Float.pp lf Float.pp rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_notequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_notequal: different bitwidth"
      else
        let* lf = NumericValue.value_float lv in
        let* rf = NumericValue.value_float rv in
        let rv = if Float.compare lf rf <> 0 then 1L else 0L in
        [%log finfo "float" "%a <> %a = %Lx" Float.pp lf Float.pp rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_less ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_less: different bitwidth"
      else
        let* lf = NumericValue.value_float lv in
        let* rf = NumericValue.value_float rv in
        let rv = if Float.compare lf rf < 0 then 1L else 0L in
        [%log finfo "float" "%a < %a = %Lx" Float.pp lf Float.pp rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_lessequal ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_lessequal: different bitwidth"
      else
        let* lf = NumericValue.value_float lv in
        let* rf = NumericValue.value_float rv in
        let rv = if Float.compare lf rf <= 0 then 1L else 0L in
        [%log finfo "float" "%a <= %a = %Lx" Float.pp lf Float.pp rf rv];
        NumericValue.of_int64_safe rv outwidth
  | Bfloat_add ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_add: different bitwidth"
      else
        let* lf = NumericValue.value_float lv in
        let* rf = NumericValue.value_float rv in
        let fv = Float.add lf rf in
        [%log finfo "float" "%a + %a = %a" Float.pp lf Float.pp rf Float.pp fv];
        NumericValue.of_float fv outwidth |> Result.ok
  | Bfloat_sub ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_sub: different bitwidth"
      else
        let* lf = NumericValue.value_float lv in
        let* rf = NumericValue.value_float rv in
        let fv = Float.sub lf rf in
        [%log finfo "float" "%a - %a = %a" Float.pp lf Float.pp rf Float.pp fv];
        NumericValue.of_float fv outwidth |> Result.ok
  | Bfloat_mult ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_mult: different bitwidth"
      else
        let* lf = NumericValue.value_float lv in
        let* rf = NumericValue.value_float rv in
        let fv = Float.mul lf rf in
        [%log finfo "float" "%a * %a = %a" Float.pp lf Float.pp rf Float.pp fv];
        NumericValue.of_float fv outwidth |> Result.ok
  | Bfloat_div ->
      if NumericValue.width lv <> NumericValue.width rv then
        Error "float_div: different bitwidth"
      else
        let* lf = NumericValue.value_float lv in
        let* rf = NumericValue.value_float rv in
        let fv = Float.div lf rf in
        [%log finfo "float" "%a / %a = %a" Float.pp lf Float.pp rf Float.pp fv];
        NumericValue.of_float fv outwidth |> Result.ok
