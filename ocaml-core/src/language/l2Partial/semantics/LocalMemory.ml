open Basic
open Basic_collection
open Common_language

let ( let* ) = Result.bind

module FuncTimestampMap = Map.Make (struct
  type t = Loc.t * Int64.t

  let compare = compare
end)

type t = Frame.t FuncTimestampMap.t

let empty = FuncTimestampMap.empty
let add = FuncTimestampMap.add
let remove = FuncTimestampMap.remove

let load_mem (s : t) (addr : SPVal.t) (width : Int32.t) : Value.t =
  if FuncTimestampMap.mem (addr.func, addr.timestamp) s then
    Frame.load_mem
      (FuncTimestampMap.find (addr.func, addr.timestamp) s)
      addr.offset width
  else NonNum (NonNumericValue.undefined width)

let load_string (s : t) (addr : SPVal.t) : (String.t, String.t) Result.t =
  if FuncTimestampMap.mem (addr.func, addr.timestamp) s then
    Frame.load_string
      (FuncTimestampMap.find (addr.func, addr.timestamp) s)
      addr.offset
  else Ok ""

let store_mem (s : t) (addr : SPVal.t) (v : Value.t) : (t, String.t) Result.t =
  match FuncTimestampMap.find_opt (addr.func, addr.timestamp) s with
  | Some frame ->
      let* nf = Frame.store_mem frame addr.offset v in
      FuncTimestampMap.add (addr.func, addr.timestamp) nf s |> Result.ok
  | None -> "store_mem: frame not found" |> Result.error
