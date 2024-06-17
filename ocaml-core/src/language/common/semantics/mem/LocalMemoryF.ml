open StdlibExt
open Notation

module type S = sig
  module Value : ValueF.S
  module Frame : FrameF.S with module Value = Value

  type t

  val empty : t
  val add : Loc.t * Int64.t -> Frame.t -> t -> t
  val remove : Loc.t * Int64.t -> t -> t
  val load_mem : t -> SPVal.t -> Int32.t -> Value.t
  val load_string : t -> SPVal.t -> (String.t, String.t) Result.t
  val store_mem : t -> SPVal.t -> Value.t -> (t, String.t) Result.t
  val load_bytes : t -> SPVal.t -> Int32.t -> (String.t, String.t) Result.t
  val store_bytes : t -> SPVal.t -> String.t -> (t, String.t) Result.t
end

module Make (Value : ValueF.S) (Frame : FrameF.S with module Value = Value) =
struct
  module Value = Value
  module Frame = Frame

  type t = Frame.t FuncTimestampMap.t

  let empty = FuncTimestampMap.empty
  let add = FuncTimestampMap.add
  let remove = FuncTimestampMap.remove

  let load_mem (s : t) (addr : SPVal.t) (width : Int32.t) : Value.t =
    if FuncTimestampMap.mem (addr.func, addr.timestamp) s then
      Frame.load_mem
        (FuncTimestampMap.find (addr.func, addr.timestamp) s)
        addr.offset width
    else Value.zero width

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
