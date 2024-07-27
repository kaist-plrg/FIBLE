type left_e = LE of Int64.t | MInf
type right_e = RE of Int64.t | Inf
type t = V of (left_e * right_e) | Bot

let top = V (MInf, Inf)
let bot = Bot

let join_left (x : left_e) (y : left_e) : left_e =
  match (x, y) with LE x, LE y -> LE (Int64.min x y) | _ -> MInf

let meet_left (x : left_e) (y : left_e) : left_e =
  match (x, y) with
  | LE x, LE y -> LE (Int64.max x y)
  | LE x, MInf -> LE x
  | MInf, LE x -> LE x
  | _ -> MInf

let join_right (x : right_e) (y : right_e) : right_e =
  match (x, y) with RE x, RE y -> RE (Int64.max x y) | _ -> Inf

let meet_right (x : right_e) (y : right_e) : right_e =
  match (x, y) with
  | RE x, RE y -> RE (Int64.min x y)
  | RE x, Inf -> RE x
  | Inf, RE x -> RE x
  | _ -> Inf

let join (x : t) (y : t) : t =
  match (x, y) with
  | Bot, x -> x
  | x, Bot -> x
  | V (xlow, xhigh), V (ylow, yhigh) ->
      V (join_left xlow ylow, join_right xhigh yhigh)

let meet (x : t) (y : t) : t =
  match (x, y) with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | V (xlow, xhigh), V (ylow, yhigh) ->
      V (meet_left xlow ylow, meet_right xhigh yhigh)

let le (x : t) (y : t) : bool =
  match (x, y) with
  | Bot, _ -> true
  | _, Bot -> false
  | V (xlow, xhigh), V (ylow, yhigh) ->
      let ole_low =
        match (xlow, ylow) with
        | LE x, LE y -> y <= x
        | LE _, MInf -> true
        | MInf, LE _ -> false
        | _ -> true
      in
      let ole_high =
        match (xhigh, yhigh) with
        | RE x, RE y -> x <= y
        | RE _, Inf -> true
        | Inf, RE _ -> false
        | _ -> true
      in
      ole_low && ole_high

let pp_left fmt x =
  match x with
  | LE x -> Format.fprintf fmt "%Ld" x
  | MInf -> Format.fprintf fmt "-∞"

let pp_right fmt x =
  match x with
  | RE x -> Format.fprintf fmt "%Ld" x
  | Inf -> Format.fprintf fmt "+∞"

let pp fmt (x : t) =
  match x with
  | V (xlow, xhigh) -> Format.fprintf fmt "[%a, %a]" pp_left xlow pp_right xhigh
  | Bot -> Format.fprintf fmt "⊥"

let of_const (x : int64) : t = V (LE x, RE x)

let mul (x : t) (y : t) : t =
  match (x, y) with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | V (LE xlow, RE xhigh), V (LE ylow, RE yhigh) ->
      if xlow >= 0L && ylow >= 0L then
        V (LE (Int64.mul xlow ylow), RE (Int64.mul xhigh yhigh))
      else top
  | _ -> top

let add (x : t) (y : t) : t =
  match (x, y) with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | V (LE xlow, RE xhigh), V (LE ylow, RE yhigh) ->
      V (LE (Int64.add xlow ylow), RE (Int64.add xhigh yhigh))
  | _ -> top

let sext (x : t) (_ : int32) (out_width : int32) =
  match x with
  | Bot -> Bot
  | V (LE xlow, RE xhigh) ->
      if xlow >= Int64.shift_left 1L (Int32.to_int (Int32.sub out_width 1l))
      then top
      else if
        xhigh >= Int64.shift_left 1L (Int32.to_int (Int32.sub out_width 1l))
      then top
      else V (LE xlow, RE xhigh)
  | _ -> top

let try_concretize (x : t) (limit : int) : Int64Set.t option =
  match x with
  | Bot -> None
  | V (LE xlow, RE xhigh) ->
      if Int64.sub xhigh xlow > Int64.of_int limit then None
      else
        Int64Set.of_list
          (List.init
             (Int64.to_int (Int64.sub xhigh xlow) + 1)
             (fun i -> Int64.add xlow (Int64.of_int i)))
        |> Option.some
  | _ -> None

let make (x : left_e) (y : right_e) : t =
  match (x, y) with
  | LE x, RE y -> if x <= y then V (LE x, RE y) else Bot
  | x, y -> V (x, y)
