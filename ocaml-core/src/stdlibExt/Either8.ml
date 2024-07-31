type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t =
  | First of 'a
  | Second of 'b
  | Third of 'c
  | Fourth of 'd
  | Fifth of 'e
  | Sixth of 'f
  | Seventh of 'g
  | Eighth of 'h

let first x = First x
let second x = Second x
let third x = Third x
let fourth x = Fourth x
let fifth x = Fifth x
let sixth x = Sixth x
let seventh x = Seventh x
let eighth x = Eighth x
let is_first = function First _ -> true | _ -> false
let is_second = function Second _ -> true | _ -> false
let is_third = function Third _ -> true | _ -> false
let is_fourth = function Fourth _ -> true | _ -> false
let is_fifth = function Fifth _ -> true | _ -> false
let is_sixth = function Sixth _ -> true | _ -> false
let is_seventh = function Seventh _ -> true | _ -> false
let is_eighth = function Eighth _ -> true | _ -> false
let find_first = function First x -> Some x | _ -> None
let find_second = function Second x -> Some x | _ -> None
let find_third = function Third x -> Some x | _ -> None
let find_fourth = function Fourth x -> Some x | _ -> None
let find_fifth = function Fifth x -> Some x | _ -> None
let find_sixth = function Sixth x -> Some x | _ -> None
let find_seventh = function Seventh x -> Some x | _ -> None
let find_eighth = function Eighth x -> Some x | _ -> None

let fold first second third fourth fifth sixth seventh eighth = function
  | First x -> first x
  | Second x -> second x
  | Third x -> third x
  | Fourth x -> fourth x
  | Fifth x -> fifth x
  | Sixth x -> sixth x
  | Seventh x -> seventh x
  | Eighth x -> eighth x

let iter first second third fourth fifth sixth seventh eighth :
    (_, _, _, _, _, _, _, _) t -> unit =
  fold first second third fourth fifth sixth seventh eighth

let equal first second third fourth fifth sixth seventh eighth a b =
  match (a, b) with
  | First x, First y -> first x y
  | Second x, Second y -> second x y
  | Third x, Third y -> third x y
  | Fourth x, Fourth y -> fourth x y
  | Fifth x, Fifth y -> fifth x y
  | Sixth x, Sixth y -> sixth x y
  | Seventh x, Seventh y -> seventh x y
  | Eighth x, Eighth y -> eighth x y
  | _ -> false

let compare first second third fourth fifth sixth seventh eighth a b =
  match (a, b) with
  | First x, First y -> first x y
  | Second x, Second y -> second x y
  | Third x, Third y -> third x y
  | Fourth x, Fourth y -> fourth x y
  | Fifth x, Fifth y -> fifth x y
  | Sixth x, Sixth y -> sixth x y
  | Seventh x, Seventh y -> seventh x y
  | Eighth x, Eighth y -> eighth x y
  | First _, _ -> -1
  | _, First _ -> 1
  | Second _, _ -> -1
  | _, Second _ -> 1
  | Third _, _ -> -1
  | _, Third _ -> 1
  | Fourth _, _ -> -1
  | _, Fourth _ -> 1
  | Fifth _, _ -> -1
  | _, Fifth _ -> 1
  | Sixth _, _ -> -1
  | _, Sixth _ -> 1
  | Seventh _, _ -> -1
  | _, Seventh _ -> 1
