type t = FailStop of String.t | NormalStop [@@deriving sexp]

let of_str_res (res : ('a, String.t) result) : ('a, t) Result.t =
  match res with Ok x -> Ok x | Error e -> Error (FailStop e)

let add_loc (v : ('a, t) Result.t) (loc : Loc.t) : ('a, t) Result.t =
  match v with
  | Ok x -> Ok x
  | Error (FailStop e) ->
      Error (FailStop (Format.asprintf "%a: %s" Loc.pp loc e))
  | Error NormalStop -> Error NormalStop

let map_error (v : ('a, t) Result.t) (f : String.t -> String.t) :
    ('a, t) Result.t =
  match v with
  | Ok x -> Ok x
  | Error (FailStop e) -> Error (FailStop (f e))
  | Error NormalStop -> Error NormalStop
