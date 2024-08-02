module Make (Value : sig
  type t
end) =
struct
  type value_t = Value.t

  type t =
    | Assign of RegId.t_full * Value.t
    | Load of RegId.t_full * Value.t * Value.t
    | Store of Value.t * Value.t
    | Special of String.t
    | Nop

  let of_assign r v = Assign (r, v)
  let of_load r p v = Load (r, p, v)
  let of_store p v = Store (p, v)
  let of_special s = Special s
  let nop = Nop

  let to_either5 (v : t) :
      ( RegId.t_full * Value.t,
        RegId.t_full * Value.t * Value.t,
        Value.t * Value.t,
        String.t,
        Unit.t )
      Either5.t =
    match v with
    | Assign (r, v) -> First (r, v)
    | Load (r, p, v) -> Second (r, p, v)
    | Store (p, v) -> Third (p, v)
    | Special s -> Fourth s
    | Nop -> Fifth ()
end
