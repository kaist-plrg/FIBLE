include List

let take (n : int) (xs : 'a list) : 'a list =
  let rec aux (n : int) (xs : 'a list) (acc : 'a list) : 'a list =
    match (n, xs) with
    | 0, _ | _, [] -> rev acc
    | _, x :: xs -> aux (n - 1) xs (x :: acc)
  in
  aux n xs []

let hd_opt (xs : 'a list) : 'a option =
  match xs with [] -> None | x :: _ -> Some x
