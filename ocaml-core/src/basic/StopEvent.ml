type t = FailStop of String.t | NormalStop

let of_str_res (res : ('a, String.t) result) : ('a, t) result =
  match res with Ok x -> Ok x | Error e -> Error (FailStop e)
