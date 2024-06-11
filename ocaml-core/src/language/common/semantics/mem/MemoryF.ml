open StdlibExt
open Notation

module type S = sig
  module Value : ValueF.S
  module TimeStamp : TimeStampF.S
  module Frame : FrameF.S with module Value = Value

  type t

  val load_mem : t -> Value.pointer_t -> Int32.t -> Value.t
  val load_string : t -> Value.pointer_t -> (String.t, String.t) Result.t

  val load_bytes :
    t -> Value.pointer_t -> Int32.t -> (String.t, String.t) Result.t

  val store_mem : t -> Value.pointer_t -> Value.t -> (t, String.t) Result.t
  val store_bytes : t -> Value.pointer_t -> String.t -> (t, String.t) Result.t
  val add_local_frame : Loc.t * TimeStamp.t -> Frame.t -> t -> t
end

module Make
    (Value : ValueF.S)
    (TimeStamp : TimeStampF.S)
    (Frame : FrameF.S with module Value = Value)
    (GlobalMemory : GlobalMemoryF.S with module Value = Value)
    (LocalMemory : LocalMemoryF.S
                     with module Value = Value
                      and module Frame = Frame) =
struct
  module Value = Value
  module TimeStamp = TimeStamp
  module Frame = Frame

  type t = Unit.t

  let of_global_memory (m : GlobalMemory.t) : t = ()

  let load_mem (m : t) (ptr : Value.pointer_t) (width : Int32.t) : Value.t =
    Value.zero width

  let load_string (m : t) (ptr : Value.pointer_t) :
      (String.t, String.t) Result.t =
    Ok ""

  let load_bytes (m : t) (ptr : Value.pointer_t) (width : Int32.t) :
      (String.t, String.t) Result.t =
    Ok ""

  let store_mem (m : t) (ptr : Value.pointer_t) (v : Value.t) :
      (t, String.t) Result.t =
    Ok ()

  let store_bytes (m : t) (ptr : Value.pointer_t) (v : String.t) :
      (t, String.t) Result.t =
    Ok ()

  let add_local_frame (loc : Loc.t * TimeStamp.t) (frame : Frame.t) (m : t) : t
      =
    ()
end
