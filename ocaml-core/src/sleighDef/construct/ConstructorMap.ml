open StdlibExt
open Notation

type 'operand_t poly_t = 'operand_t TypeDef.constructor_map_poly_t
type t = TypeDef.constructor_map_unmapped
type ptr_t = TypeDef.constructor_map_ptr_t

let cardinal (v : 'a poly_t) = Int32Map.cardinal v.map

let of_list (l : 'a Constructor.poly_t List.t) (id : Int32.t) : 'a poly_t =
  let _, map =
    List.fold_left
      (fun (i, map) c -> (Int32.succ i, Int32Map.add i c map))
      (0l, Int32Map.empty) l
  in
  { id; map }

let get_constructor (v : 'a poly_t) (ptr : ConstructorPtr.t) :
    ('a Constructor.poly_t, String.t) Result.t =
  if v.id == ConstructorPtr.get_table_id ptr then
    Int32Map.find_opt (ConstructorPtr.get_offset ptr) v.map
    |> Option.to_result
         ~none:
           (Format.sprintf "Not found constructor at %ld"
              (ConstructorPtr.get_offset ptr))
  else "table id and ptr id not matched" |> Result.error

let find_offset (v : 'a poly_t) (offset : Int32.t) :
    ('a Constructor.poly_t, String.t) Result.t =
  Int32Map.find_opt offset v.map
  |> Option.to_result
       ~none:(Format.sprintf "Not found constructor at %ld" offset)
