type t = Int64.t

val add : t -> t -> t
val pp : Format.formatter -> t -> unit
val scan : Scanf.Scanning.in_channel -> t
val of_int32 : Int32.t -> t
val of_int64 : Int64.t -> t
val get_offset : t -> Int64.t
val get_addr_size : t -> Int.t
val compare : t -> t -> Int.t
val pred : t -> t
val succ : t -> t
