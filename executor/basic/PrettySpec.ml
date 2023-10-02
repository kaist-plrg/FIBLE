module type PrettyPrint = sig
  type t

  val pp : Format.formatter -> t -> unit
end
