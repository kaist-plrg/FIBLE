module Make (VarNode : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (CallTarget : sig
  type t

  val pp : Format.formatter -> t -> unit
  val get_loc_opt : t -> Loc.t option
end) (JTailCall : sig
  type t

  module Attr : sig
    type t
  end

  val get_target : t -> CallTarget.t
  val get_attr : t -> Attr.t
end) (Value : sig
  type t

  val try_loc : t -> (Loc.t, String.t) Result.t
end) (Store : sig
  type t

  val eval_vn : t -> VarNode.t -> (Value.t, String.t) Result.t
end) (SCallTarget : sig
  type t

  val eval : Store.t -> CallTarget.t -> (t, String.t) Result.t
end) (Attr : sig
  type t

  val eval : Store.t -> JTailCall.Attr.t -> (t, String.t) Result.t
end) =
struct
  type t = { target : SCallTarget.t; attr : Attr.t }

  let get_target { target; _ } = target
  let get_attr { attr; _ } = attr

  let eval (s : Store.t) (j : JTailCall.t) =
    let* target = SCallTarget.eval s (JTailCall.get_target j) in
    let* attr = Attr.eval s (JTailCall.get_attr j) in
    { target; attr } |> Result.ok
end
