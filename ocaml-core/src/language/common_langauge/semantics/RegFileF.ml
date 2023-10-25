open Basic
open Basic_collection

module Make (Value : sig
  type t

  val zero : Int32.t -> t
  val refine_width : t -> Int32.t -> t
  val replace_width : t -> t -> Int32.t -> t
end) =
struct
  include RegIdMap

  type t = Value.t RegIdMap.t

  let get_reg (s : t) (r : RegId.t_width) : Value.t =
    Value.refine_width
      (RegIdMap.find_opt r.id s |> Option.value ~default:(Value.zero r.width))
      r.width

  let add_reg (s : t) (r : RegId.t_width) (v : Value.t) : t =
    RegIdMap.add r.id
      (Value.replace_width (get_reg s { id = r.id; width = 8l }) v r.width)
      s
end
