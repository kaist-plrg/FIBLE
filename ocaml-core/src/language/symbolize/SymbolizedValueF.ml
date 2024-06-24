open Common

module MakeNonnum (NonNumericValue : sig
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
  val sp : SPVal.t -> t
  val get_sp : t -> SPVal.t option
end) =
struct
  type t = Global of Unit.t | NonGlobal of NonNumericValue.t

  let pp fmt v =
    match v with
    | Global u -> Format.fprintf fmt "global()"
    | NonGlobal v -> NonNumericValue.pp fmt v

  let eval_uop op v w =
    match v with
    | Global _ -> Either.left (NumericValue.undefined w)
    | NonGlobal v -> NonNumericValue.eval_uop op v w

  let eval_bop op v w =
    match v with
    | Either3.First (NonGlobal v1, NonGlobal v2) ->
        NonNumericValue.eval_bop op (Either3.First (v1, v2)) w
        |> Either.map_right (fun v -> NonGlobal v)
    | Either3.Second (NonGlobal v1, v2) ->
        NonNumericValue.eval_bop op (Either3.Second (v1, v2)) w
        |> Either.map_right (fun v -> NonGlobal v)
    | Either3.Third (v1, NonGlobal v2) ->
        NonNumericValue.eval_bop op (Either3.Third (v1, v2)) w
        |> Either.map_right (fun v -> NonGlobal v)
    | _ -> Either.left (NumericValue.undefined w)

  let width v =
    match v with Global _ -> 8l | NonGlobal v -> NonNumericValue.width v

  let undefined w = NonGlobal (NonNumericValue.undefined w)
  let sp v = NonGlobal (NonNumericValue.sp v)

  let get_sp v =
    match v with Global _ -> None | NonGlobal v -> NonNumericValue.get_sp v
end
