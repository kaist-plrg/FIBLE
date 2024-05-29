type ('a, 'b, 'c, 'd, 'e, 'f) t =
  | First of 'a
  | Second of 'b
  | Third of 'c
  | Fourth of 'd
  | Fifth of 'e
  | Sixth of 'f

let first x = First x
let second x = Second x
let third x = Third x
let fourth x = Fourth x
let fifth x = Fifth x
let sixth x = Sixth x
let is_first = function First _ -> true | _ -> false
let is_second = function Second _ -> true | _ -> false
let is_third = function Third _ -> true | _ -> false
let is_fourth = function Fourth _ -> true | _ -> false
let is_fifth = function Fifth _ -> true | _ -> false
let is_sixth = function Sixth _ -> true | _ -> false
let find_first = function First x -> Some x | _ -> None
let find_second = function Second x -> Some x | _ -> None
let find_third = function Third x -> Some x | _ -> None
let find_fourth = function Fourth x -> Some x | _ -> None
let find_fifth = function Fifth x -> Some x | _ -> None
let find_sixth = function Sixth x -> Some x | _ -> None

let fold first second third fourth fifth sixth = function
  | First x -> first x
  | Second x -> second x
  | Third x -> third x
  | Fourth x -> fourth x
  | Fifth x -> fifth x
  | Sixth x -> sixth x

let iter first second third fourth fifth sixth : (_, _, _, _, _, _) t -> unit =
  fold first second third fourth fifth sixth

let equal first second third fourth fifth sixth a b =
  match (a, b) with
  | First x, First y -> first x y
  | Second x, Second y -> second x y
  | Third x, Third y -> third x y
  | Fourth x, Fourth y -> fourth x y
  | Fifth x, Fifth y -> fifth x y
  | Sixth x, Sixth y -> sixth x y
  | _ -> false

let compare first second third fourth fifth sixth a b =
  match (a, b) with
  | First x, First y -> first x y
  | Second x, Second y -> second x y
  | Third x, Third y -> third x y
  | Fourth x, Fourth y -> fourth x y
  | Fifth x, Fifth y -> fifth x y
  | Sixth x, Sixth y -> sixth x y
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
