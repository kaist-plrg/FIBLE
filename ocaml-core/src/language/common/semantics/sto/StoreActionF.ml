module Make (Value : sig
  type t

  val pp : Format.formatter -> t -> Unit.t
end) =
struct
  type value_t = Value.t

  type t =
    | Assign of RegId.t_full * Value.t
    | Load of RegId.t_full * Value.t * Value.t
    | Store of Value.t * Value.t
    | Special of String.t * (Value.t * Bytes.t) List.t * Interop.t List.t
    | Nop
  [@@deriving show]

  let of_assign r v = Assign (r, v)
  let of_load r p v = Load (r, p, v)
  let of_store p v = Store (p, v)
  let of_special s l vs = Special (s, l, vs)
  let nop = Nop

  let to_either5 (v : t) :
      ( RegId.t_full * Value.t,
        RegId.t_full * Value.t * Value.t,
        Value.t * Value.t,
        String.t * (Value.t * Bytes.t) List.t * Interop.t List.t,
        Unit.t )
      Either5.t =
    match v with
    | Assign (r, v) -> First (r, v)
    | Load (r, p, v) -> Second (r, p, v)
    | Store (p, v) -> Third (p, v)
    | Special (s, l, vs) -> Fourth (s, l, vs)
    | Nop -> Fifth ()
end
