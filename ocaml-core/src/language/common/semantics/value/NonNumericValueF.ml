module type S = sig
  type t

  val width : t -> Int32.t
  val undefined : Int32.t -> t
  val pp : Format.formatter -> t -> unit
  val eval_uop : Uop.t -> t -> Int32.t -> (NumericValue.t, t) Either.t

  val eval_bop :
    Bop.t ->
    (t * t, t * NumericValue.t, NumericValue.t * t) Either3.t ->
    Int32.t ->
    (NumericValue.t, t) Either.t

  val get : t -> Int32.t -> Int32.t -> t
  val sp : SPVal.t -> t
  val get_sp : t -> SPVal.t option
end
