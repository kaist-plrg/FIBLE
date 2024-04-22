open StdlibExt
open Notation

type t = { id : Int32.t; map : Constructor.t Int32Map.t }

let cardinal (v : t) = Int32Map.cardinal v.map

let of_list (l : Constructor.t List.t) (id : Int32.t) : t =
  let _, map =
    List.fold_left
      (fun (i, map) c -> (Int32.succ i, Int32Map.add i c map))
      (0l, Int32Map.empty) l
  in
  { id; map }

let get_constructor (v : t) (ptr : ConstructorPtr.t) :
    (Constructor.t, String.t) Result.t =
  if v.id == ConstructorPtr.get_table_id ptr then
    Int32Map.find_opt (ConstructorPtr.get_offset ptr) v.map
    |> Option.to_result
         ~none:
           (Format.sprintf "Not found constructor at %ld"
              (ConstructorPtr.get_offset ptr))
  else "table id and ptr id not matched" |> Result.error

let find_offset (v : t) (offset : Int32.t) : (Constructor.t, String.t) Result.t
    =
  Int32Map.find_opt offset v.map
  |> Option.to_result
       ~none:(Format.sprintf "Not found constructor at %ld" offset)
