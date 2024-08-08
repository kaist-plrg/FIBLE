module type S = sig
  module Value : sig
    type t

    val zero : Int32.t -> t
    val undefined : Int32.t -> t
    val get : t -> Int32.t -> Int32.t -> t
    val extend : t -> Int32.t -> t
    val extend_undef : t -> Int32.t -> t
    val set : t -> t -> Int32.t -> t
    val pp : Format.formatter -> t -> unit
  end

  type t

  val empty : Int32.t Int32Map.t -> t
  val get_reg : t -> RegId.t_full -> Value.t
  val add_reg : t -> RegId.t_full -> Value.t -> t
  val pp : Format.formatter -> t -> unit
end

module Make (Value : sig
  type t

  val zero : Int32.t -> t
  val undefined : Int32.t -> t
  val get : t -> Int32.t -> Int32.t -> t
  val extend : t -> Int32.t -> t
  val extend_undef : t -> Int32.t -> t
  val set : t -> t -> Int32.t -> t
  val pp : Format.formatter -> t -> unit
end) =
struct
  include RegIdMap
  module Value = Value

  type t = Value.t RegIdMap.t

  let empty (spec : Int32.t Int32Map.t) =
    Int32Map.fold
      (fun rid width acc -> RegIdMap.add (Register rid) (Value.zero width) acc)
      spec RegIdMap.empty

  let get_reg (s : t) (r : RegId.t_full) : Value.t =
    RegIdMap.find_opt r.id s
    |> Option.map (Fun.flip Value.extend_undef (Int32.add r.offset r.width))
    |> Option.map (fun v -> Value.get v r.offset r.width)
    |> Option.value ~default:(Value.undefined r.width)

  let add_reg (s : t) (r : RegId.t_full) (v : Value.t) : t =
    let v =
      RegIdMap.find_opt r.id s
      |> Option.map (Fun.flip Value.extend_undef (Int32.add r.offset r.width))
      |> Option.value ~default:(Value.undefined r.width)
      |> fun o -> Value.set o v r.offset
    in
    RegIdMap.add r.id v s

  let pp fmt (s : t) =
    RegIdMap.iter
      (fun k v -> Format.fprintf fmt "%a: %a@ " RegId.pp k Value.pp v)
      s
end
