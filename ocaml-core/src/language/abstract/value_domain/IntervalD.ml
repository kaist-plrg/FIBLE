type left_e = LE of Z.t | MInf
type right_e = RE of Z.t | Inf
type t = V of (left_e * right_e) | Bot

let top = V (MInf, Inf)
let bot = Bot

let join_left (x : left_e) (y : left_e) : left_e =
  match (x, y) with LE x, LE y -> LE (Z.min x y) | _ -> MInf

let meet_left (x : left_e) (y : left_e) : left_e =
  match (x, y) with
  | LE x, LE y -> LE (Z.max x y)
  | LE x, MInf -> LE x
  | MInf, LE x -> LE x
  | _ -> MInf

let join_right (x : right_e) (y : right_e) : right_e =
  match (x, y) with RE x, RE y -> RE (Z.max x y) | _ -> Inf

let meet_right (x : right_e) (y : right_e) : right_e =
  match (x, y) with
  | RE x, RE y -> RE (Z.min x y)
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
  | LE x -> Format.fprintf fmt "%a" Z.pp_print x
  | MInf -> Format.fprintf fmt "-∞"

let pp_right fmt x =
  match x with
  | RE x -> Format.fprintf fmt "%a" Z.pp_print x
  | Inf -> Format.fprintf fmt "+∞"

let pp fmt (x : t) =
  match x with
  | V (xlow, xhigh) -> Format.fprintf fmt "[%a, %a]" pp_left xlow pp_right xhigh
  | Bot -> Format.fprintf fmt "⊥"

let of_const (x : Z.t) : t = V (LE x, RE x)

let mul (x : t) (y : t) : t =
  match (x, y) with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | V (LE xlow, RE xhigh), V (LE ylow, RE yhigh) ->
      if xlow >= Z.zero && ylow >= Z.zero then
        V (LE (Z.mul xlow ylow), RE (Z.mul xhigh yhigh))
      else top
  | _ -> top

let add (x : t) (y : t) : t =
  match (x, y) with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | V (LE xlow, RE xhigh), V (LE ylow, RE yhigh) ->
      V (LE (Z.add xlow ylow), RE (Z.add xhigh yhigh))
  | _ -> top

let sext (x : t) (_ : int32) (out_width : int32) =
  match x with
  | Bot -> Bot
  | V (LE xlow, RE xhigh) ->
      if xlow >= Z.shift_left Z.one (Int32.to_int (Int32.sub out_width 1l)) then
        top
      else if
        xhigh >= Z.shift_left Z.one (Int32.to_int (Int32.sub out_width 1l))
      then top
      else V (LE xlow, RE xhigh)
  | _ -> top

let try_concretize (x : t) (limit : int) : Int64Set.t option =
  match x with
  | Bot -> None
  | V (LE xlow, RE xhigh) ->
      if Z.sub xhigh xlow > Z.of_int limit then None
      else
        Int64Set.of_list
          (List.init
             (Z.to_int (Z.sub xhigh xlow) + 1)
             (fun i -> Z.add xlow (Z.of_int i))
          |> List.map Z.to_int64)
        |> Option.some
  | _ -> None

let make (x : left_e) (y : right_e) : t =
  match (x, y) with
  | LE x, RE y -> if x <= y then V (LE x, RE y) else Bot
  | x, y -> V (x, y)
