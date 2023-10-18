open Basic
open Basic_collection

include AddrMap
type t = Char.t AddrMap.t

let load_mem (s : t) (addr : Addr.t) (width : Int32.t) : Value.t =
  let rec aux (addr : Addr.t) (width : Int32.t) (acc : Char.t list) :
      Char.t list =
    if width = 0l then acc
    else
      let c =
        AddrMap.find_opt addr s |> Option.value ~default:(Char.chr 0)
      in
      aux (Addr.succ addr) (Int32.pred width) (c :: acc)
  in
  let chars = aux addr width [] |> List.rev in
  Value.of_chars chars

let store_mem (s : t) (addr : Addr.t) (v : Value.t) : Char.t AddrMap.t =
  let chars = Value.to_chars v in
  let rec aux (addr : Addr.t) (chars : Char.t list) (acc : Char.t AddrMap.t) :
      Char.t AddrMap.t =
    match chars with
    | [] -> acc
    | c :: chars ->
        let acc = AddrMap.add addr c acc in
        aux (Addr.succ addr) chars acc
  in
  aux addr chars s
