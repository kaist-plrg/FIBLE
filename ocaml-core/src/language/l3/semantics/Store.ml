open Basic
open Basic_collection
open Common_language

type t = { regs : RegFile.t; mem : Memory.t; local : LocalMemory.t }

let add_reg (s : t) (r : RegId.t_width) (v : Value.t) : t =
  { s with regs = RegFile.add_reg s.regs r v }

let get_reg (s : t) (r : RegId.t_width) : Value.t = RegFile.get_reg s.regs r

let load_mem_global (s : t) (addr : Addr.t) (width : Int32.t) : Value.t =
  Memory.load_mem s.mem addr width

let load_string_global (s : t) (addr : Addr.t) : (String.t, String.t) Result.t =
  Memory.load_string s.mem addr

let store_mem_global (s : t) (addr : Addr.t) (v : Value.t) : t =
  { s with mem = Memory.store_mem s.mem addr v }

let load_mem_local (s : t) (addr : LPVal.t) (width : Int32.t) : Value.t =
  LocalMemory.load_mem_local s.local addr width

let load_string_local (s : t) (addr : LPVal.t) : (String.t, String.t) Result.t =
  LocalMemory.load_string_local s.local addr

let store_mem_local (s : t) (addr : LPVal.t) (v : Value.t) : t =
  { s with local = LocalMemory.store_mem_local s.local addr v }

let load_mem_param (s : t) (addr : PPVal.t) (width : Int32.t) : Value.t =
  LocalMemory.load_mem_param s.local addr width

let load_string_param (s : t) (addr : PPVal.t) : (String.t, String.t) Result.t =
  LocalMemory.load_string_param s.local addr

let store_mem_param (s : t) (addr : PPVal.t) (v : Value.t) : t =
  { s with local = LocalMemory.store_mem_param s.local addr v }

let load_mem (s : t) (v : Value.t) (width : Int32.t) :
    (Value.t, String.t) Result.t =
  match v with
  | Num adv ->
      let addr = NumericValue.to_addr adv in
      Ok (load_mem_global s addr width)
  | NonNum (ParamP pv) -> Ok (load_mem_param s pv width)
  | NonNum (LocalP lv) -> Ok (load_mem_local s lv width)
  | NonNum (SP sv) -> Ok (NonNum (Undef width))
  | NonNum (Undef _) -> Error "load: Undefined address"

let load_string (s : t) (v : Value.t) : (String.t, String.t) Result.t =
  match v with
  | Num adv ->
      let addr = NumericValue.to_addr adv in
      load_string_global s addr
  | NonNum (ParamP pv) -> load_string_param s pv
  | NonNum (LocalP lv) -> load_string_local s lv
  | NonNum (SP sv) -> Error (Format.asprintf "load: SP %a" SPVal.pp sv)
  | NonNum (Undef _) -> Error "load: Undefined address"

let store_mem (s : t) (v : Value.t) (e : Value.t) : (t, String.t) Result.t =
  match v with
  | Num adv ->
      let addr = NumericValue.to_addr adv in
      Ok (store_mem_global s addr e)
  | NonNum (ParamP pv) -> Ok (store_mem_param s pv e)
  | NonNum (LocalP lv) -> Ok (store_mem_local s lv e)
  | NonNum (SP sv) -> Error (Format.asprintf "store: SP %a" SPVal.pp sv)
  | NonNum (Undef _) -> Error "store: Undefined address"
