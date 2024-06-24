module Make (Value : sig
  type t
end) (Store : sig
  type t

  val add_reg : t -> RegId.t_full -> Value.t -> t
  val get_reg : t -> RegId.t_full -> Value.t
  val load_mem : t -> Value.t -> Int32.t -> (Value.t, String.t) Result.t
  val load_string : t -> Value.t -> (String.t, String.t) Result.t
  val load_bytes : t -> Value.t -> Int32.t -> (String.t, String.t) Result.t
  val store_mem : t -> Value.t -> Value.t -> (t, String.t) Result.t
  val store_bytes : t -> Value.t -> String.t -> (t, String.t) Result.t
end) (State : sig
  type t

  val get_store : t -> Store.t
  val set_store : t -> Store.t -> t
end) =
struct
  let add_reg state reg value =
    let store = State.get_store state in
    let store' = Store.add_reg store reg value in
    State.set_store state store'

  let get_reg state reg =
    let store = State.get_store state in
    Store.get_reg store reg

  let load_mem state addr width =
    let store = State.get_store state in
    Store.load_mem store addr width

  let load_string state addr =
    let store = State.get_store state in
    Store.load_string store addr

  let load_bytes state addr width =
    let store = State.get_store state in
    Store.load_bytes store addr width

  let store_mem state addr value =
    let store = State.get_store state in
    Store.store_mem store addr value
    |> Result.map (fun store' -> State.set_store state store')

  let store_bytes state addr value =
    let store = State.get_store state in
    Store.store_bytes store addr value
    |> Result.map (fun store' -> State.set_store state store')
end
