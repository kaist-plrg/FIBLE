open StdlibExt.Notation

module Make (CallTarget : sig
  type t

  val pp : Format.formatter -> t -> unit
  val get_loc_opt : t -> Loc.t option
end) (JCall : sig
  type t

  module Attr : sig
    type t
  end

  val get_target : t -> CallTarget.t
  val get_fallthrough : t -> Loc.t
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

  val eval : Store.t -> JCall.Attr.t -> (t, String.t) Result.t
end) =
struct
  type t = { target : SCallTarget.t; fallthrough : Loc.t; attr : Attr.t }

  let get_target { target; _ } = target
  let get_fallthrough { fallthrough; _ } = fallthrough
  let get_attr { attr; _ } = attr

  let eval (s : Store.t) (j : JCall.t) =
    let* target = SCallTarget.eval s (JCall.get_target j) in
    let* attr = Attr.eval s (JCall.get_attr j) in
    { target; fallthrough = JCall.get_fallthrough j; attr } |> Result.ok
end
