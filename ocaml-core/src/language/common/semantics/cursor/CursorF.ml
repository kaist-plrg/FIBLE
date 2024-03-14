module Make (TimeStamp : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = { func : Loc.t; tick : TimeStamp.t }

  let pp fmt { func; tick } =
    Format.fprintf fmt "%a@%a" Loc.pp func TimeStamp.pp tick

  let get_func_loc (v : t) = v.func
  let get_timestamp (v : t) = v.tick
  let make (func : Loc.t) (tick : TimeStamp.t) = { func; tick }
end
