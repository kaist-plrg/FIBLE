type t

val compare : t -> t -> Int.t
val pp : Format.formatter -> t -> Unit.t
val scan : Scanf.Scanning.in_channel -> t
val of_addr : Byte8.t -> t
val of_addr_seq : Byte8.t * Int.t -> t
val get_addr : t -> Byte8.t
val get_seq : t -> Int.t
val to_addr_seq : t -> Byte8.t * Int.t
