open StdlibExt
open Basic
open Basic_collection

module Make (Value : sig
  type t

  val zero : Int32.t -> t
  val get : t -> Int32.t -> Int32.t -> t
  val extend : t -> Int32.t -> t
  val set : t -> t -> Int32.t -> t
end) =
struct
  include RegIdMap

  type t = Value.t RegIdMap.t

  let empty (spec : Int32.t Int32Map.t) =
    Int32Map.fold
      (fun rid width acc -> RegIdMap.add (Register rid) (Value.zero width) acc)
      spec RegIdMap.empty

  let get_reg (s : t) (r : RegId.t_full) : Value.t =
    RegIdMap.find_opt r.id s
    |> Option.map (Fun.flip Value.extend (Int32.add r.offset r.width))
    |> Option.map (fun v -> Value.get v r.offset r.width)
    |> Option.value ~default:(Value.zero r.width)

  let add_reg (s : t) (r : RegId.t_full) (v : Value.t) : t =
    let v =
      RegIdMap.find_opt r.id s
      |> Option.map (Fun.flip Value.extend (Int32.add r.offset r.width))
      |> Option.value ~default:(Value.zero r.width)
      |> fun o -> Value.set o v r.offset
    in
    RegIdMap.add r.id v s
end
