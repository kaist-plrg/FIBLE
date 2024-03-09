open StdlibExt
open Notation
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

  val width : t -> Int32.t
  val undefined : Int32.t -> t
  val get_sp : t -> SPVal.t option
end) =
struct
  module NonNumericValue = NonNumericValue

  type t = Num of NumericValue.t | NonNum of NonNumericValue.t

  let of_num (n : NumericValue.t) : t = Num n
  let of_nonnum (n : NonNumericValue.t) : t = NonNum n

  let to_either (v : t) : (NumericValue.t, NonNumericValue.t) Either.t =
    match v with Num n -> Left n | NonNum n -> Right n

  let of_either (v : (NumericValue.t, NonNumericValue.t) Either.t) : t =
    match v with Left n -> Num n | Right n -> NonNum n

  let get_space (v : t) : (NumericValue.t, SPVal.t, Unit.t) Either3.t =
    match v with
    | Num n -> First n
    | NonNum n -> (
        match NonNumericValue.get_sp n with
        | Some sp -> Second sp
        | None -> Third ())

  let zero w = Num (NumericValue.zero w)

  let width (v : t) : Int32.t =
    match v with
    | Num n -> NumericValue.width n
    | NonNum n -> NonNumericValue.width n

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

  let get (x : t) (offset : Int32.t) (size : Int32.t) : t =
    match x with
    | Num n -> Num (NumericValue.get n offset size)
    | NonNum n ->
        if Int32.equal offset Int32.zero && size = NonNumericValue.width n then
          NonNum n
        else NonNum (NonNumericValue.undefined size)

  let extend (x : t) (size : Int32.t) =
    match x with
    | Num n -> Num (NumericValue.extend n size)
    | NonNum n ->
        if Int32.equal size (NonNumericValue.width n) then NonNum n
        else NonNum (NonNumericValue.undefined size)

  let set (orig : t) (inserted : t) (offset : Int32.t) =
    match (orig, inserted) with
    | Num orig, Num inserted -> Num (NumericValue.set orig inserted offset)
    | NonNum orig, Num inserted ->
        if Int32.equal offset Int32.zero then
          Num (NumericValue.extend inserted (NonNumericValue.width orig))
        else NonNum (NonNumericValue.undefined (NonNumericValue.width orig))
    | Num orig, NonNum inserted ->
        if
          Int32.equal offset Int32.zero
          && NumericValue.width orig = NonNumericValue.width inserted
        then NonNum inserted
        else NonNum (NonNumericValue.undefined (NonNumericValue.width inserted))
    | NonNum orig, NonNum inserted ->
        if
          Int32.equal offset Int32.zero
          && NonNumericValue.width orig = NonNumericValue.width inserted
        then NonNum inserted
        else NonNum (NonNumericValue.undefined (NonNumericValue.width inserted))
end
