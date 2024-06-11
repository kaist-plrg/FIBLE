module type S = sig
  type t

  val pp : Format.formatter -> t -> unit
  val succ : t -> t
end
