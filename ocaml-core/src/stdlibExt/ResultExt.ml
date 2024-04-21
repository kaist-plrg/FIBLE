open Notation

let fold_left_M (f : 'acc -> 'a -> ('acc, 'e) Result.t) (init : 'acc)
    (l : 'a list) : ('acc, 'e) Result.t =
  List.fold_left
    (fun accr x ->
      let* acc = accr in
      f acc x)
    (init |> Result.ok) l

let join_list (l : ('a, 'e) Result.t List.t) : ('a List.t, 'e) Result.t =
  let* l =
    List.fold_left
      (fun acc x ->
        Result.bind acc (fun acc ->
            Result.bind x (fun x -> Result.Ok (x :: acc))))
      (Result.Ok []) l
  in
  Result.Ok (List.rev l)
