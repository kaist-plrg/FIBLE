type t = FailStop of String.t | NormalStop

let of_str_res (res : ('a, String.t) result) : ('a, t) Result.t =
  match res with Ok x -> Ok x | Error e -> Error (FailStop e)

let add_loc (v : ('a, t) Result.t) (loc : Loc.t) : ('a, t) Result.t =
  match v with
  | Ok x -> Ok x
  | Error (FailStop e) ->
      Error (FailStop (Format.asprintf "%a: %s" Loc.pp loc e))
  | Error NormalStop -> Error NormalStop
