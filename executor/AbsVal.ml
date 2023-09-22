type t = IntervalD.t

let top = IntervalD.top

let join = IntervalD.join
let meet = IntervalD.meet
let ole = IntervalD.ole

let pp fmt a = IntervalD.pp fmt a

let of_const n = top

let of_interval x = top


let add a b = top
let sub a b = top
let mul a b = top