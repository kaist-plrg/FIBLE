open Basic
open Basic_collection

module Make (Value : sig
  type t

  val zero : Int32.t -> t
end) =
struct
  include RegIdMap

  type t = Value.t RegIdMap.t

  let add_reg (s : t) (r : RegId.t_width) (v : Value.t) : t =
    RegIdMap.add r.id v s

  let get_reg (s : t) (r : RegId.t_width) : Value.t =
    RegIdMap.find_opt r.id s |> Option.value ~default:(Value.zero r.width)
end
