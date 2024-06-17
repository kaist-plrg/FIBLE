module type S = sig
  type t

  val get_width : t -> int32
  val pp : Format.formatter -> t -> unit
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end
