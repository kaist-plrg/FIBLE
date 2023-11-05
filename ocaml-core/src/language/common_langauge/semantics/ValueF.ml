open StdlibExt
open Basic

module Make (NonNumericValue : sig
  type t

  val pp : Format.formatter -> t -> unit
  val eval_uop : Uop.t -> t -> Int32.t -> (NumericValue.t, t) Either.t

  val eval_bop :
    Bop.t ->
    (t * t, t * NumericValue.t, NumericValue.t * t) Either3.t ->
    Int32.t ->
    (NumericValue.t, t) Either.t

  val refine_width : t -> Int32.t -> t

  val width_of: t -> Int32.t
  val undefined: Int32.t -> t
end) =
struct
  let ( let* ) = Result.bind
  module NonNumericValue = NonNumericValue

  type t = Num of NumericValue.t | NonNum of NonNumericValue.t
  type nonnum_t = NonNumericValue.t

  let to_either (v : t) : (NumericValue.t, NonNumericValue.t) Either.t =
    match v with Num n -> Left n | NonNum n -> Right n

  let of_either (v : (NumericValue.t, NonNumericValue.t) Either.t) : t =
    match v with Left n -> Num n | Right n -> NonNum n

  let zero w = Num (NumericValue.zero w)

  let pp fmt = function
    | Num n -> NumericValue.pp fmt n
    | NonNum n -> NonNumericValue.pp fmt n

  let try_addr (v : t) : (Addr.t, String.t) Result.t =
    match v with
    | Num n -> Ok (NumericValue.to_addr n)
    | NonNum n -> Error (Format.asprintf "try_addr: %a" NonNumericValue.pp n)

  let try_loc (v : t) : (Loc.t, String.t) Result.t =
    match v with
    | Num n -> Ok (NumericValue.to_loc n)
    | NonNum n -> Error (Format.asprintf "try_loc: %a" NonNumericValue.pp n)

  let try_isZero (v : t) : (Bool.t, String.t) Result.t =
    match v with
    | Num n -> Ok (NumericValue.isZero n)
    | NonNum n -> Error (Format.asprintf "try_isZero: %a" NonNumericValue.pp n)

  let eval_uop (u : Uop.t) (v : t) (outwidth : Int32.t) : (t, String.t) Result.t
      =
    match v with
    | Num vn ->
        let* vn' = NumericUop.eval u vn outwidth in
        Ok (Num vn')
    | NonNum nvn -> Ok (NonNumericValue.eval_uop u nvn outwidth |> of_either)

  let eval_bop (b : Bop.t) (lv : t) (rv : t) (outwidth : Int32.t) :
      (t, String.t) Result.t =
    match (lv, rv) with
    | Num lv, Num rv ->
        let* vn' = NumericBop.eval b lv rv outwidth in
        Ok (Num vn')
    | Num lv, NonNum rv ->
        let vn' =
          NonNumericValue.eval_bop b (Third (lv, rv)) outwidth |> of_either
        in
        Ok vn'
    | NonNum lv, Num rv ->
        let vn' =
          NonNumericValue.eval_bop b (Second (lv, rv)) outwidth |> of_either
        in
        Ok vn'
    | NonNum lv, NonNum rv ->
        let vn' =
          NonNumericValue.eval_bop b (First (lv, rv)) outwidth |> of_either
        in
        Ok vn'

  let refine_width (v : t) (width : int32) : t =
    match v with
    | Num n -> Num (NumericValue.refine_width n width)
    | NonNum n -> NonNum (NonNumericValue.refine_width n width)

  let replace_width (ov : t) (nv : t) (width : Int32.t) : t =
    match (ov, nv) with
    | Num ov, Num nv -> Num (NumericValue.replace_width ov nv width)
    | _, Num nv -> Num nv
    | _, NonNum nv -> NonNum (NonNumericValue.refine_width nv width)
end
