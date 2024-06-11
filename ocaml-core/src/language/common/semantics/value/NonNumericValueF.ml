module type S = sig
  type t

  val width : t -> Int32.t
  val undefined : Int32.t -> t
end
