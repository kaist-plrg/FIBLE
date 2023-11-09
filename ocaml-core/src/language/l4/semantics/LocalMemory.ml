open Basic
open Basic_collection
open Common_language

type loctype = Param | Local

module FuncTimestampMap = Map.Make (struct
  type t = loctype * Loc.t * Int64.t

  let compare = compare
end)

type t = Frame.t FuncTimestampMap.t

let empty = FuncTimestampMap.empty
let add = FuncTimestampMap.add
let remove = FuncTimestampMap.remove

let load_mem_param (s : t) (addr : PPVal.t) (width : Int32.t) : Value.t =
  if FuncTimestampMap.mem (Param, addr.func, addr.timestamp) s then
    Frame.load_mem
      (FuncTimestampMap.find (Param, addr.func, addr.timestamp) s)
      addr.offset width
  else Num (NumericValue.zero width)

let load_mem_local (s : t) (addr : LPVal.t) (width : Int32.t) : Value.t =
  if FuncTimestampMap.mem (Local, addr.func, addr.timestamp) s then
    Frame.load_mem
      (FuncTimestampMap.find (Local, addr.func, addr.timestamp) s)
      addr.offset width
  else Num (NumericValue.zero width)

let load_string_param (s : t) (addr : PPVal.t) : (String.t, String.t) Result.t =
  if FuncTimestampMap.mem (Param, addr.func, addr.timestamp) s then
    Frame.load_string
      (FuncTimestampMap.find (Param, addr.func, addr.timestamp) s)
      addr.offset
  else Ok ""

let load_string_local (s : t) (addr : LPVal.t) : (String.t, String.t) Result.t =
  if FuncTimestampMap.mem (Local, addr.func, addr.timestamp) s then
    Frame.load_string
      (FuncTimestampMap.find (Local, addr.func, addr.timestamp) s)
      addr.offset
  else Ok ""

let store_mem_param (s : t) (addr : PPVal.t) (v : Value.t) : t =
  if FuncTimestampMap.mem (Param, addr.func, addr.timestamp) s then
    FuncTimestampMap.add
      (Param, addr.func, addr.timestamp)
      (Frame.store_mem
         (FuncTimestampMap.find (Param, addr.func, addr.timestamp) s)
         addr.offset v)
      s
  else
    FuncTimestampMap.add
      (Param, addr.func, addr.timestamp)
      (Frame.store_mem Frame.empty addr.offset v)
      s

let store_mem_local (s : t) (addr : LPVal.t) (v : Value.t) : t =
  if FuncTimestampMap.mem (Local, addr.func, addr.timestamp) s then
    FuncTimestampMap.add
      (Local, addr.func, addr.timestamp)
      (Frame.store_mem
         (FuncTimestampMap.find (Local, addr.func, addr.timestamp) s)
         addr.offset v)
      s
  else
    FuncTimestampMap.add
      (Local, addr.func, addr.timestamp)
      (Frame.store_mem Frame.empty addr.offset v)
      s
