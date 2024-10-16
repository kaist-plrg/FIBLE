module Make (Value : sig
  type t

  val pp : Format.formatter -> t -> Unit.t
end) (StoreAction : sig
  type t

  val of_assign : RegId.t_full -> Value.t -> t
  val of_load : RegId.t_full -> Value.t -> Value.t -> t
  val of_store : Value.t -> Value.t -> t
  val nop : t
  val pp : Format.formatter -> t -> Unit.t
end) =
struct
  module StoreAction = StoreAction

  type t =
    | StoreAction of StoreAction.t * Loc.t
    | Jmp of Loc.t
    | ExternCall of Loc.t
  [@@deriving show]

  let jmp l = Jmp l
  let externcall l = ExternCall l
  let of_store s l = StoreAction (s, l)
end
