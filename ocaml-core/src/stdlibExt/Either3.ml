type ('a, 'b, 'c) t = First of 'a | Second of 'b | Third of 'c

let first x = First x
let second x = Second x
let third x = Third x
let is_first = function First _ -> true | _ -> false
let is_second = function Second _ -> true | _ -> false
let is_third = function Third _ -> true | _ -> false
let find_first = function First x -> Some x | _ -> None
let find_second = function Second x -> Some x | _ -> None
let find_third = function Third x -> Some x | _ -> None

let fold first second third : ('a, 'b, 'c) t -> 'd = function
  | First x -> first x
  | Second x -> second x
  | Third x -> third x

let iter first second third : ('a, 'b, 'c) t -> Unit.t = fold first second third

let equal first second third : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> bool =
 fun a b ->
  match (a, b) with
  | First x, First y -> first x y
  | Second x, Second y -> second x y
  | Third x, Third y -> third x y
  | _ -> false

let compare first second third : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int =
 fun a b ->
  match (a, b) with
  | First x, First y -> first x y
  | Second x, Second y -> second x y
  | Third x, Third y -> third x y
  | First _, _ -> -1
  | Second _, First _ -> 1
  | Second _, _ -> -1
  | Third _, _ -> 1
