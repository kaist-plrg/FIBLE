type t = MemoryBlock.t List.t [@@deriving sexp]

let get_byte (memory : t) (address : Int64.t) : Char.t =
  List.fold_left
    (fun acc block ->
      match acc with
      | Some _ -> acc
      | None -> MemoryBlock.get_byte block address)
    None memory
  |> Option.value ~default:(Char.chr 0)

let get_numeric (rom : t) (addr : Byte8.t) (width : Int32.t) : NumericValue.t =
  let rec aux (addr : Byte8.t) (width : Int32.t) (acc : Char.t list) :
      Char.t list =
    if width = 0l then acc
    else
      let c = get_byte rom addr in
      aux (Byte8.succ addr) (Int32.pred width) (c :: acc)
  in
  let chars = aux addr width [] |> List.rev in
  NumericValue.of_chars chars
