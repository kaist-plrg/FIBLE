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
  | EInt x, EInt y -> if x = y then EInt x else if (Int64.min x y < -10L) then ETop else EInt (Int64.min x y)
  | _ -> ETop

let meet_et_low (x : et) (y : et) : et =
  match x, y with
  | EInt x, EInt y -> if x = y then EInt x else EInt (Int64.max x y)
  | EInt x, ETop -> EInt x
  | ETop, EInt x -> EInt x
  | _ -> ETop

let join_et_high (x : et) (y : et) : et =
  match x, y with
  | EInt x, EInt y -> if x = y then EInt x else if (Int64.max x y > 10L) then ETop else EInt (Int64.max x y)
  | _ -> ETop

let meet_et_high (x : et) (y : et) : et =
  match x, y with
  | EInt x, EInt y -> if x = y then EInt x else EInt (Int64.min x y)
  | EInt x, ETop -> EInt x
  | ETop, EInt x -> EInt x
  | _ -> ETop


let join (x : t) (y : t) : t =
  match x, y with
  | (xlow, xhigh), (ylow, yhigh) -> (join_et_low xlow ylow, join_et_high xhigh yhigh)

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
  | (xlow, xhigh) -> Format.fprintf fmt "(%a, %a)" pp_et xlow pp_et xhigh