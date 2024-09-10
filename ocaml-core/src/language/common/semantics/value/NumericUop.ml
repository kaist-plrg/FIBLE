let eval (u : Uop.t) (v : NumericValue.t) (outwidth : Int32.t) :
    (NumericValue.t, String.t) Result.t =
  match u with
  | Upopcount ->
      let* n = NumericValue.value_64 v in
      NumericValue.of_int64_safe
        (Int64.cut_width (Int64.bitcount n) outwidth)
        outwidth
  | Ulzcount ->
      let* n = NumericValue.value_64 v in
      NumericValue.of_int64_safe
        (Int64.sub (Int64.of_int32 (NumericValue.width v)) (Int64.bitwidth n))
        outwidth
  | Uint_zext ->
      let* n = NumericValue.value_64 v in
      NumericValue.of_int64_safe
        (Int64.zext n (NumericValue.width v) outwidth)
        outwidth
  | Uint_sext ->
      let* n = NumericValue.value_64 v in
      NumericValue.of_int64_safe
        (Int64.sext n (NumericValue.width v) outwidth)
        outwidth
  | Uint_2comp ->
      let* n = NumericValue.value_64 v in
      NumericValue.of_int64_safe
        (Int64.cut_width (Int64.neg n) outwidth)
        outwidth
  | Uint_negate ->
      let* n = NumericValue.value_64 v in
      NumericValue.of_int64_safe
        (Int64.cut_width (Int64.lognot n) outwidth)
        outwidth
  | Ubool_negate ->
      let* n = NumericValue.value_64 v in
      NumericValue.of_int64_safe
        (Int64.cut_width (if Int64.equal n 0L then 1L else 0L) outwidth)
        outwidth
  | Ufloat_neg ->
      let* f = NumericValue.value_float v in
      NumericValue.of_float (Float.neg f) outwidth |> Result.ok
  | Ufloat_abs ->
      let* f = NumericValue.value_float v in
      NumericValue.of_float (Float.abs f) outwidth |> Result.ok
  | Ufloat_sqrt ->
      let* f = NumericValue.value_float v in
      NumericValue.of_float (Float.sqrt f) outwidth |> Result.ok
  | Ufloat_ceil ->
      let* f = NumericValue.value_float v in
      NumericValue.of_float (Float.ceil f) outwidth |> Result.ok
  | Ufloat_floor ->
      let* f = NumericValue.value_float v in
      NumericValue.of_float (Float.floor f) outwidth |> Result.ok
  | Ufloat_round ->
      let* f = NumericValue.value_float v in
      NumericValue.of_float (Float.round f) outwidth |> Result.ok
  | Ufloat_nan ->
      let* f = NumericValue.value_float v in
      NumericValue.of_z (if Float.is_nan f then Z.one else Z.zero) outwidth
      |> Result.ok
  | Uint2float ->
      let* n = NumericValue.value_signed_z v in
      NumericValue.of_float
        (Float.read (Z.to_string n) (outwidth |> Int32.to_int))
        outwidth
      |> Result.ok
  | Ufloat2float ->
      let* f = NumericValue.value_float v in
      NumericValue.of_float
        (Float.read (Float.show f) (outwidth |> Int32.to_int))
        outwidth
      |> Result.ok
  | Utrunc ->
      let* f = NumericValue.value_float v in
      let n = Float.trunc f in
      NumericValue.of_z n outwidth |> Result.ok
