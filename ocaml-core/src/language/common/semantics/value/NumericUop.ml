let eval (u : Uop.t) (v : NumericValue.t) (outwidth : Int32.t) :
    (NumericValue.t, String.t) Result.t =
  let* n = NumericValue.value_64 v in
  match u with
  | Upopcount ->
      NumericValue.of_int64_safe
        (Int64.cut_width (Int64.bitcount n) outwidth)
        outwidth
  | Ulzcount ->
      NumericValue.of_int64_safe
        (Int64.sub (Int64.of_int32 (NumericValue.width v)) (Int64.bitwidth n))
        outwidth
  | Uint_zext ->
      NumericValue.of_int64_safe
        (Int64.zext n (NumericValue.width v) outwidth)
        outwidth
  | Uint_sext ->
      NumericValue.of_int64_safe
        (Int64.sext n (NumericValue.width v) outwidth)
        outwidth
  | Uint_2comp ->
      NumericValue.of_int64_safe
        (Int64.cut_width (Int64.neg n) outwidth)
        outwidth
  | Uint_negate ->
      NumericValue.of_int64_safe
        (Int64.cut_width (Int64.lognot n) outwidth)
        outwidth
  | Ubool_negate ->
      NumericValue.of_int64_safe
        (Int64.cut_width (if Int64.equal n 0L then 1L else 0L) outwidth)
        outwidth
  | Ufloat_neg ->
      let* fv = Int64.float_neg n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64.cut_width fv outwidth) outwidth
  | Ufloat_abs ->
      let* fv = Int64.float_abs n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64.cut_width fv outwidth) outwidth
  | Ufloat_sqrt ->
      let* fv = Int64.float_sqrt n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64.cut_width fv outwidth) outwidth
  | Ufloat_ceil ->
      let* fv = Int64.float_ceil n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64.cut_width fv outwidth) outwidth
  | Ufloat_floor ->
      let* fv = Int64.float_floor n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64.cut_width fv outwidth) outwidth
  | Ufloat_round ->
      let* fv = Int64.float_round n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64.cut_width fv outwidth) outwidth
  | Ufloat_nan ->
      let* fv = Int64.float_round n (NumericValue.width v) in
      NumericValue.of_int64_safe (Int64.cut_width fv outwidth) outwidth
  | Uint2float ->
      let* fv = Int64.int2float n (NumericValue.width v) outwidth in
      NumericValue.of_int64_safe (Int64.cut_width fv outwidth) outwidth
  | Ufloat2float ->
      let* fv = Int64.float2float n (NumericValue.width v) outwidth in
      NumericValue.of_int64_safe (Int64.cut_width fv outwidth) outwidth
  | Utrunc ->
      let* fv = Int64.trunc n (NumericValue.width v) outwidth in
      NumericValue.of_int64_safe (Int64.cut_width fv outwidth) outwidth
