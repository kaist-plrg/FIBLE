type et =
 | EInt of int64
 | ETop


let pp_et fmt (x : et) =
  match x with
  | EInt x -> Format.fprintf fmt "%Ld" x
  | ETop -> Format.fprintf fmt "T"
  
type t = (et * et)

let top = (ETop, ETop)

let join_et_low (x : et) (y : et) : et =
  match x, y with
  | EInt x, EInt y -> if x = y then EInt x else EInt (Int64.min x y)
  | _ -> ETop

let meet_et_low (x : et) (y : et) : et =
  match x, y with
  | EInt x, EInt y -> if x = y then EInt x else EInt (Int64.max x y)
  | EInt x, ETop -> EInt x
  | ETop, EInt x -> EInt x
  | _ -> ETop

let join_et_high (x : et) (y : et) : et =
  match x, y with
  | EInt x, EInt y -> if x = y then EInt x else EInt (Int64.max x y)
  | _ -> ETop

let meet_et_high (x : et) (y : et) : et =
  match x, y with
  | EInt x, EInt y -> if x = y then EInt x else EInt (Int64.min x y)
  | EInt x, ETop -> EInt x
  | ETop, EInt x -> EInt x
  | _ -> ETop


let join (x : t) (y : t) : t =
  match x, y with
  | (xlow, xhigh), (ylow, yhigh) -> let nt =
      (join_et_low xlow ylow, join_et_high xhigh yhigh) in
      match nt with
      | (EInt x, EInt y) -> if Int64.sub y x > 100L then top else nt
      | (EInt x, ETop) -> if x < -100L then top else nt
      | (ETop, EInt x) -> if x > 100L then top else nt
      | _ -> nt

let meet (x : t) (y : t) : t =
  match x, y with
  | (xlow, xhigh), (ylow, yhigh) -> (meet_et_low xlow ylow, meet_et_high xhigh yhigh)

let ole (x : t) (y : t) : bool =
  match x, y with
  | (xlow, xhigh), (ylow, yhigh) ->
    let ole_low = (match xlow, ylow with
    | EInt x, EInt y -> y <= x
    | EInt x, ETop -> true
    | ETop, EInt x -> false
    | _ -> true) in
    let ole_high = (match xhigh, yhigh with
    | EInt x, EInt y -> x <= y
    | EInt x, ETop -> true
    | ETop, EInt x -> false
    | _ -> true) in
    ole_low && ole_high

let pp fmt (x : t) =
  match x with
  | (xlow, xhigh) -> Format.fprintf fmt "[%a, %a]" pp_et xlow pp_et xhigh


let of_const (x : int64) : t =
  (EInt x, EInt x)

let mul (x : t) (y : t) : t =
  match x, y with
  | (EInt xlow, EInt xhigh), (EInt ylow, EInt yhigh) -> if xlow >= 0L && ylow >= 0L then  (EInt (Int64.mul xlow ylow), EInt (Int64.mul xhigh yhigh)) else top
  |  _ -> top

let add (x : t) (y : t) : t =
  match x, y with
  | (EInt xlow, EInt xhigh), (EInt ylow, EInt yhigh) -> (EInt (Int64.add xlow ylow), EInt (Int64.add xhigh yhigh))
  |  _ -> top

let sext (x: t) (in_width: int32) (out_width: int32) =
  match x with
  | (EInt xlow, EInt xhigh) -> if xlow >= (Int64.shift_left 1L (Int32.to_int (Int32.sub out_width 1l))) then top else
    if xhigh >= (Int64.shift_left 1L (Int32.to_int (Int32.sub out_width 1l))) then top else
      (EInt xlow, EInt xhigh)
  | _ -> top

let try_concretize (x: t) (limit: int): DS.Int64Set.t option = match x with
  | (EInt xlow, EInt xhigh) ->
    if Int64.sub xhigh xlow > Int64.of_int limit then None
    else
      DS.Int64Set.of_list (List.init (Int64.to_int (Int64.sub xhigh xlow) + 1) (fun i -> Int64.add xlow (Int64.of_int i))) |> Option.some
  | _ -> None