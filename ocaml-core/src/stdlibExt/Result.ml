include Stdlib.Result

let fold_left_M (f : 'acc -> 'a -> ('acc, 'e) t) (init : 'acc) (l : 'a list) :
    ('acc, 'e) t =
  List.fold_left (fun accr x -> bind accr (fun acc -> f acc x)) (init |> ok) l

let join_list (l : ('a, 'e) t List.t) : ('a List.t, 'e) t =
  bind
    (List.fold_left
       (fun acc x -> bind acc (fun acc -> bind x (fun x -> Ok (x :: acc))))
       (Ok []) l)
    (fun l -> Ok (List.rev l))
