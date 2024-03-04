open Basic
open Basic_collection
open Common_language

type t = { mem : Memory.t; local : LocalMemory.t }

let load_mem_global (s : t) (addr : Addr.t) (width : Int32.t) : Value.t =
  Memory.load_mem s.mem addr width

let load_string_global (s : t) (addr : Addr.t) : (String.t, String.t) Result.t =
  Memory.load_string s.mem addr

let store_mem_global (s : t) (addr : Addr.t) (v : Value.t) : t =
  { s with mem = Memory.store_mem s.mem addr v }

let load_mem_local (s : t) (addr : SPVal.t) (width : Int32.t) : Value.t =
  LocalMemory.load_mem s.local addr width

let load_string_local (s : t) (addr : SPVal.t) : (String.t, String.t) Result.t =
  LocalMemory.load_string s.local addr

let store_mem_local (s : t) (addr : SPVal.t) (v : Value.t) : t =
  { s with local = LocalMemory.store_mem s.local addr v }

let load_mem (s : t) (v : Value.t) (width : Int32.t) :
    (Value.t, String.t) Result.t =
  match v with
  | Num adv ->
      let addr = NumericValue.to_addr adv in
      Ok (load_mem_global s addr width)
  | NonNum (SP sv) -> Ok (load_mem_local s sv width)
  | NonNum (Undef _) -> Error "load: Undefined address"

let load_string (s : t) (v : Value.t) : (String.t, String.t) Result.t =
  match v with
  | Num adv ->
      let addr = NumericValue.to_addr adv in
      load_string_global s addr
  | NonNum (SP sv) -> load_string_local s sv
  | NonNum (Undef _) -> Error "load: Undefined address"

let load_bytes (s : t) (v : Value.t) (width : Int32.t) :
    (String.t, String.t) Result.t =
  match v with
  | Num adv ->
      let addr = NumericValue.to_addr adv in
      Memory.load_bytes s.mem addr width
  | NonNum (SP sv) -> LocalMemory.load_bytes s.local sv width
  | NonNum (Undef _) -> Error "load: Undefined address"

let store_mem (s : t) (v : Value.t) (e : Value.t) : (t, String.t) Result.t =
  match v with
  | Num adv ->
      let addr = NumericValue.to_addr adv in
      Ok (store_mem_global s addr e)
  | NonNum (SP sv) -> Ok (store_mem_local s sv e)
  | NonNum (Undef _) -> Error "store: Undefined address"

let store_bytes (s : t) (v : Value.t) (e : String.t) : (t, String.t) Result.t =
  match v with
  | Num adv ->
      let addr = NumericValue.to_addr adv in
      Ok { s with mem = Memory.store_bytes s.mem addr e }
  | NonNum (SP sv) -> Ok { s with local = LocalMemory.store_bytes s.local sv e }
  | NonNum (Undef _) -> Error "store: Undefined address"
