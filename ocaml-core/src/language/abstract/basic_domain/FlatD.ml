module Make (A : sig
  type t

  val pp : Format.formatter -> t -> unit
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
    | Flat x, Flat y -> if x = y then Flat x else Top

  let meet a b =
    match (a, b) with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Top, x -> x
    | x, Top -> x
    | Flat x, Flat y -> if x = y then Flat x else Bot

  let widen a b = join a b

  let le a b =
    match (a, b) with
    | Top, _ -> true
    | _, Top -> false
    | Bot, _ -> true
    | _, Bot -> false
    | Flat x, Flat y -> x = y

  let pp fmt = function
    | Top -> Format.fprintf fmt "Top"
    | Bot -> Format.fprintf fmt "Bot"
    | Flat x -> Format.fprintf fmt "%a" A.pp x
end

