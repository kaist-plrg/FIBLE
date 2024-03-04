open Basic
open Basic_collection
open Common_language

type loctype = Param | Local

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
  else Num (NumericValue.zero width)

let load_string (s : t) (addr : SPVal.t) : (String.t, String.t) Result.t =
  if FuncTimestampMap.mem (addr.func, addr.timestamp) s then
    Frame.load_string
      (FuncTimestampMap.find (addr.func, addr.timestamp) s)
      addr.offset
  else Ok ""

let store_mem (s : t) (addr : SPVal.t) (v : Value.t) : t =
  if FuncTimestampMap.mem (addr.func, addr.timestamp) s then
    FuncTimestampMap.add
      (addr.func, addr.timestamp)
      (Frame.store_mem
         (FuncTimestampMap.find (addr.func, addr.timestamp) s)
         addr.offset v)
      s
  else
    FuncTimestampMap.add
      (addr.func, addr.timestamp)
      (Frame.store_mem Frame.empty addr.offset v)
      s

let load_bytes (s : t) (addr : SPVal.t) (len : Int32.t) :
    (String.t, String.t) Result.t =
  if FuncTimestampMap.mem (addr.func, addr.timestamp) s then
    Frame.load_bytes
      (FuncTimestampMap.find (addr.func, addr.timestamp) s)
      addr.offset len
  else Ok ""

let store_bytes (s : t) (addr : SPVal.t) (v : String.t) : t =
  if FuncTimestampMap.mem (addr.func, addr.timestamp) s then
    FuncTimestampMap.add
      (addr.func, addr.timestamp)
      (Frame.store_bytes
         (FuncTimestampMap.find (addr.func, addr.timestamp) s)
         addr.offset v)
      s
  else
    FuncTimestampMap.add
      (addr.func, addr.timestamp)
      (Frame.store_bytes Frame.empty addr.offset v)
      s
