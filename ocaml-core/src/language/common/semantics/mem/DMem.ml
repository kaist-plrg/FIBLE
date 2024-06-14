open Sexplib.Std

type t = MemoryBlock.t list [@@deriving sexp]

let get_byte (memory : t) (address : Int64.t) : Char.t =
  List.fold_left
    (fun acc block ->
      match acc with
      | Some _ -> acc
      | None -> MemoryBlock.get_byte block address)
    None memory
  |> Option.value ~default:(Char.chr 0)
