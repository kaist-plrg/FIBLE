open StdlibExt
open Notation

module Make (Value : sig
  type t

  val of_num : NumericValue.t -> t
end) (Frame : sig
  type t

  val load_mem : t -> Int64.t -> Int32.t -> Value.t
  val load_string : t -> Int64.t -> (String.t, String.t) Result.t
  val load_bytes : t -> Int64.t -> Int32.t -> (String.t, String.t) Result.t
  val store_mem : t -> Int64.t -> Value.t -> (t, String.t) Result.t
  val store_bytes : t -> Int64.t -> String.t -> (t, String.t) Result.t
end) =
struct

  type t = Frame.t FuncTimestampMap.t

  let empty = FuncTimestampMap.empty
  let add = FuncTimestampMap.add
  let remove = FuncTimestampMap.remove

  let load_mem (s : t) (addr : SPVal.t) (width : Int32.t) : Value.t =
    if FuncTimestampMap.mem (addr.func, addr.timestamp) s then
      Frame.load_mem
        (FuncTimestampMap.find (addr.func, addr.timestamp) s)
        addr.offset width
    else Value.of_num (NumericValue.zero width)

  let load_string (s : t) (addr : SPVal.t) : (String.t, String.t) Result.t =
    if FuncTimestampMap.mem (addr.func, addr.timestamp) s then
      Frame.load_string
        (FuncTimestampMap.find (addr.func, addr.timestamp) s)
        addr.offset
    else Ok ""

  let store_mem (s : t) (addr : SPVal.t) (v : Value.t) : (t, String.t) Result.t
      =
    match FuncTimestampMap.find_opt (addr.func, addr.timestamp) s with
    | Some frame ->
        let* nf = Frame.store_mem frame addr.offset v in
        FuncTimestampMap.add (addr.func, addr.timestamp) nf s |> Result.ok
    | None -> "store_mem: frame not found" |> Result.error

  let load_bytes (s : t) (addr : SPVal.t) (len : Int32.t) :
      (String.t, String.t) Result.t =
    if FuncTimestampMap.mem (addr.func, addr.timestamp) s then
      Frame.load_bytes
        (FuncTimestampMap.find (addr.func, addr.timestamp) s)
        addr.offset len
    else Ok ""

  let store_bytes (s : t) (addr : SPVal.t) (v : String.t) :
      (t, String.t) Result.t =
    match FuncTimestampMap.find_opt (addr.func, addr.timestamp) s with
    | Some frame ->
        let* nf = Frame.store_bytes frame addr.offset v in
        FuncTimestampMap.add (addr.func, addr.timestamp) nf s |> Result.ok
    | None -> "store_bytes: frame not found" |> Result.error
end
