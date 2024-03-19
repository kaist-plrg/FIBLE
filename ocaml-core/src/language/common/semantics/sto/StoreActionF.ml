open StdlibExt

module Make (Value : sig
  type t
end) =
struct
  type t =
    | Assign of RegId.t_full * Value.t
    | Load of RegId.t_full * Value.t * Value.t
    | Store of Value.t * Value.t
    | Nop

  let of_assign r v = Assign (r, v)
  let of_load r p v = Load (r, p, v)
  let of_store p v = Store (p, v)
  let nop = Nop

  let to_either4 (v : t) :
      ( RegId.t_full * Value.t,
        RegId.t_full * Value.t * Value.t,
        Value.t * Value.t,
        Unit.t )
      Either4.t =
    match v with
    | Assign (r, v) -> First (r, v)
    | Load (r, p, v) -> Second (r, p, v)
    | Store (p, v) -> Third (p, v)
    | Nop -> Fourth ()
end
