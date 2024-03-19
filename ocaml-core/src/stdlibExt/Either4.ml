type ('a, 'b, 'c, 'd) t =
  | First of 'a
  | Second of 'b
  | Third of 'c
  | Fourth of 'd

let first x = First x
let second x = Second x
let third x = Third x
let fourth x = Fourth x
let is_first = function First _ -> true | _ -> false
let is_second = function Second _ -> true | _ -> false
let is_third = function Third _ -> true | _ -> false
let is_fourth = function Fourth _ -> true | _ -> false
let find_first = function First x -> Some x | _ -> None
let find_second = function Second x -> Some x | _ -> None
let find_third = function Third x -> Some x | _ -> None
let find_fourth = function Fourth x -> Some x | _ -> None

let map_first f = function
  | First x -> First (f x)
  | Second x -> Second x
  | Third x -> Third x
  | Fourth x -> Fourth x

let map_second f = function
  | First x -> First x
  | Second x -> Second (f x)
  | Third x -> Third x
  | Fourth x -> Fourth x

let map_third f = function
  | First x -> First x
  | Second x -> Second x
  | Third x -> Third (f x)
  | Fourth x -> Fourth x
