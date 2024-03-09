open StdlibExt
open Notation
open Basic
open Basic_collection

module Make (Value : sig
  type t

  val get_space : t -> (NumericValue.t, SPVal.t, Unit.t) Either3.t
end) (RegFile : sig
  type t

  val add_reg : t -> RegId.t_full -> Value.t -> t
  val get_reg : t -> RegId.t_full -> Value.t
end) (Memory : sig
  type t

  val load_mem : t -> Addr.t -> Int32.t -> Value.t
  val load_string : t -> Addr.t -> (String.t, String.t) Result.t
  val load_bytes : t -> Addr.t -> Int32.t -> (String.t, String.t) Result.t
  val store_mem : t -> Addr.t -> Value.t -> t
  val store_bytes : t -> Addr.t -> String.t -> t
end) (LocalMemory : sig
  type t

  val load_mem : t -> SPVal.t -> Int32.t -> Value.t
  val load_string : t -> SPVal.t -> (String.t, String.t) Result.t
  val load_bytes : t -> SPVal.t -> Int32.t -> (String.t, String.t) Result.t
  val store_mem : t -> SPVal.t -> Value.t -> (t, String.t) Result.t
  val store_bytes : t -> SPVal.t -> String.t -> (t, String.t) Result.t
end) =
struct
  type t = { regs : RegFile.t; mem : Memory.t; local : LocalMemory.t }

  let add_reg (s : t) (r : RegId.t_full) (v : Value.t) : t =
    { s with regs = RegFile.add_reg s.regs r v }

  let get_reg (s : t) (r : RegId.t_full) : Value.t = RegFile.get_reg s.regs r

  let load_mem_global (s : t) (addr : Addr.t) (width : Int32.t) : Value.t =
    Memory.load_mem s.mem addr width

  let load_string_global (s : t) (addr : Addr.t) : (String.t, String.t) Result.t
      =
    Memory.load_string s.mem addr

  let store_mem_global (s : t) (addr : Addr.t) (v : Value.t) : t =
    { s with mem = Memory.store_mem s.mem addr v }

  let load_mem_local (s : t) (addr : SPVal.t) (width : Int32.t) : Value.t =
    LocalMemory.load_mem s.local addr width

  let load_string_local (s : t) (addr : SPVal.t) : (String.t, String.t) Result.t
      =
    LocalMemory.load_string s.local addr

  let store_mem_local (s : t) (addr : SPVal.t) (v : Value.t) :
      (t, String.t) Result.t =
    let* local = LocalMemory.store_mem s.local addr v in
    { s with local } |> Result.ok

  let load_mem (s : t) (v : Value.t) (width : Int32.t) :
      (Value.t, String.t) Result.t =
    match Value.get_space v with
    | First adv ->
        let addr = NumericValue.to_addr adv in
        Ok (load_mem_global s addr width)
    | Second sv -> Ok (load_mem_local s sv width)
    | Third _ -> Error "load: Undefined address"

  let load_string (s : t) (v : Value.t) : (String.t, String.t) Result.t =
    match Value.get_space v with
    | First adv ->
        let addr = NumericValue.to_addr adv in
        load_string_global s addr
    | Second sv -> load_string_local s sv
    | Third _ -> "load: Undefined address" |> Result.error

  let store_mem (s : t) (v : Value.t) (e : Value.t) : (t, String.t) Result.t =
    match Value.get_space v with
    | First adv ->
        let addr = NumericValue.to_addr adv in
        store_mem_global s addr e |> Result.ok
    | Second sv -> store_mem_local s sv e
    | Third _ -> "store: Undefined address" |> Result.error

  let load_bytes (s : t) (v : Value.t) (width : Int32.t) :
      (String.t, String.t) Result.t =
    match Value.get_space v with
    | First adv ->
        let addr = NumericValue.to_addr adv in
        Memory.load_bytes s.mem addr width
    | Second sv -> LocalMemory.load_bytes s.local sv width
    | Third _ -> "load: Undefined address" |> Result.error

  let store_bytes (s : t) (v : Value.t) (e : String.t) : (t, String.t) Result.t
      =
    match Value.get_space v with
    | First adv ->
        let addr = NumericValue.to_addr adv in
        { s with mem = Memory.store_bytes s.mem addr e } |> Result.ok
    | Second sv ->
        let* local = LocalMemory.store_bytes s.local sv e in
        { s with local } |> Result.ok
    | Third _ -> "store: Undefined address" |> Result.error
end
