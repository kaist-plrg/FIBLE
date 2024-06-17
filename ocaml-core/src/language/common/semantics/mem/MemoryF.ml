open StdlibExt
open Notation

module type S = sig
  module Pointer : PointerF.S
  module Value : ValueF.S
  module TimeStamp : TimeStampF.S
  module Frame : FrameF.S with module Value = Value

  type t

  val load_mem : t -> Pointer.t -> Int32.t -> Value.t
  val load_string : t -> Pointer.t -> (String.t, String.t) Result.t
  val load_bytes : t -> Pointer.t -> Int32.t -> (String.t, String.t) Result.t
  val store_mem : t -> Pointer.t -> Value.t -> (t, String.t) Result.t
  val store_bytes : t -> Pointer.t -> String.t -> (t, String.t) Result.t
  val add_local_frame : Loc.t * TimeStamp.t -> Frame.t -> t -> t
end

module MakeGlobalOnly
    (Pointer : PointerF.S with type t = Byte8.t)
    (Value : ValueF.S with module Pointer = Pointer)
    (GlobalMemory : GlobalMemoryF.S with module Value = Value) =
struct
  module Pointer = Pointer
  module Value = Value

  type t = GlobalMemory.t

  let of_global_memory (m : GlobalMemory.t) : t = m

  let load_mem (m : t) (ptr : Pointer.t) (width : Int32.t) : Value.t =
    GlobalMemory.load_mem m ptr width

  let load_string (m : t) (ptr : Pointer.t) : (String.t, String.t) Result.t =
    GlobalMemory.load_string m ptr

  let load_bytes (m : t) (ptr : Pointer.t) (width : Int32.t) :
      (String.t, String.t) Result.t =
    GlobalMemory.load_bytes m ptr width

  let store_mem (m : t) (ptr : Pointer.t) (v : Value.t) : (t, String.t) Result.t
      =
    GlobalMemory.store_mem m ptr v |> Result.ok

  let store_bytes (m : t) (ptr : Pointer.t) (v : String.t) :
      (t, String.t) Result.t =
    GlobalMemory.store_bytes m ptr v |> Result.ok
end

module Make
    (Pointer : PointerF.S with type t = (Byte8.t, SPVal.t) Either.t)
    (Value : ValueF.S with module Pointer = Pointer)
    (TimeStamp : TimeStampF.S)
    (Frame : FrameF.S with module Value = Value)
    (GlobalMemory : GlobalMemoryF.S with module Value = Value)
    (LocalMemory : LocalMemoryF.S
                     with module Value = Value
                      and module Frame = Frame) =
struct
  module Pointer = Pointer
  module Value = Value
  module TimeStamp = TimeStamp
  module Frame = Frame

  type t = { global : GlobalMemory.t; local : LocalMemory.t }

  let of_global_memory (m : GlobalMemory.t) : t =
    { global = m; local = LocalMemory.empty }

  let load_mem (m : t) (ptr : Pointer.t) (width : Int32.t) : Value.t =
    match ptr with
    | Either.Left ptr -> GlobalMemory.load_mem m.global ptr width
    | Either.Right ptr -> LocalMemory.load_mem m.local ptr width

  let load_string (m : t) (ptr : Pointer.t) : (String.t, String.t) Result.t =
    match ptr with
    | Either.Left ptr -> GlobalMemory.load_string m.global ptr
    | Either.Right ptr -> LocalMemory.load_string m.local ptr

  let load_bytes (m : t) (ptr : Pointer.t) (width : Int32.t) :
      (String.t, String.t) Result.t =
    match ptr with
    | Either.Left ptr -> GlobalMemory.load_bytes m.global ptr width
    | Either.Right ptr -> LocalMemory.load_bytes m.local ptr width

  let store_mem (m : t) (ptr : Pointer.t) (v : Value.t) : (t, String.t) Result.t
      =
    match ptr with
    | Either.Left ptr ->
        GlobalMemory.store_mem m.global ptr v |> fun x ->
        Ok { m with global = x }
    | Either.Right ptr ->
        LocalMemory.store_mem m.local ptr v
        |> Result.map (fun x -> { m with local = x })

  let store_bytes (m : t) (ptr : Pointer.t) (v : String.t) :
      (t, String.t) Result.t =
    match ptr with
    | Either.Left ptr ->
        GlobalMemory.store_bytes m.global ptr v |> fun x ->
        Ok { m with global = x }
    | Either.Right ptr ->
        LocalMemory.store_bytes m.local ptr v
        |> Result.map (fun x -> { m with local = x })

  let add_local_frame (loc : Loc.t * Int64.t) (frame : Frame.t) (m : t) : t =
    { m with local = LocalMemory.add loc frame m.local }
end
