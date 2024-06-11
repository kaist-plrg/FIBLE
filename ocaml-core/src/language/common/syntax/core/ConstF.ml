module type S = sig
  type t

  val get_width : t -> int32
  val pp : Format.formatter -> t -> unit
end
