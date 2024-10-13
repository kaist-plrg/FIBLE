module Make (A : sig
  type t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end) =
struct
  type t = Top | Bot | Flat of A.t

  let top = Top
  let bot = Bot

  let join a b =
    match (a, b) with
    | Top, _ -> Top
    | _, Top -> Top
    | Bot, x -> x
    | x, Bot -> x
    | Flat x, Flat y -> if A.equal x y then Flat x else Top

  let meet a b =
    match (a, b) with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Top, x -> x
    | x, Top -> x
    | Flat x, Flat y -> if A.equal x y then Flat x else Bot

  let widen a b = join a b

  let le a b =
    match (a, b) with
    | _, Top -> true
    | Bot, _ -> true
    | _, Bot -> false
    | Top, Flat _ -> false
    | Flat x, Flat y -> A.equal x y

  let pp fmt = function
    | Top -> Format.fprintf fmt "Top"
    | Bot -> Format.fprintf fmt "Bot"
    | Flat x -> Format.fprintf fmt "%a" A.pp x
end

module MakeValue (A : sig
  type t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val eval_bop : Common.Bop.t -> t -> t -> Int32.t -> t Option.t
  val eval_uop : Common.Uop.t -> t -> Int32.t -> t Option.t
end) =
struct
  include Make (A)

  let eval_bop bop (x : t) (y : t) (width : Int32.t) : t =
    match (x, y) with
    | Flat x, Flat y -> (
        match A.eval_bop bop x y width with Some z -> Flat z | None -> Top)
    | Bot, _ | _, Bot -> Bot
    | Top, _ | _, Top -> Top

  let eval_uop uop (x : t) (width : Int32.t) : t =
    match x with
    | Flat x -> (
        match A.eval_uop uop x width with Some y -> Flat y | None -> Top)
    | Bot -> Bot
    | Top -> Top
end
