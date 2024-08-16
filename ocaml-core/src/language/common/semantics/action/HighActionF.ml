module Make (Value : sig
  type t
end) (StoreAction : sig
  type t

  val of_assign : RegId.t_full -> Value.t -> t
  val of_load : RegId.t_full -> Value.t -> Value.t -> t
  val of_store : Value.t -> Value.t -> t
  val nop : t
end) (SCall : sig
  type t
end) (STailCall : sig
  type t
end) (SRet : sig
  type t
end) =
struct
  module StoreAction = StoreAction

  type t =
    | StoreAction of (StoreAction.t * Loc.t Option.t)
    | Jmp of Loc.t
    | ExternCall of
        (String.t * (Value.t * Bytes.t) List.t * Interop.t List.t * Loc.t)
    | Call of SCall.t
    | TailCall of STailCall.t
    | Ret of SRet.t

  let of_store s lo = StoreAction (s, lo)
  let jmp l = Jmp l
  let externcall name vs ivs ft = ExternCall (name, vs, ivs, ft)
  let call j = Call j
  let tailcall j = TailCall j
  let ret j = Ret j
end
