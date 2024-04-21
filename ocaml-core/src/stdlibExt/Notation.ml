let ( let* ) = Result.bind

let ( and* ) (a : ('a, 'e) Result.t) (b : ('b, 'e) Result.t) :
    ('a * 'b, 'e) Result.t =
  match a with Ok s -> Result.map (fun b -> (s, b)) b | Error _ as e -> e
