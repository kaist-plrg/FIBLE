open StdlibExt.Notation

module Make (CallTarget : sig
  type t

  module Attr : sig
    type t
  end

  val to_either : t -> (Loc.t * Attr.t, VarNode.t) Either.t
end) (Value : sig
  type t

  val try_loc : t -> (Loc.t, String.t) Result.t
end) (Store : sig
  type t

  val eval_vn : t -> VarNode.t -> (Value.t, String.t) Result.t
end) (Attr : sig
  type t

  val eval : Store.t -> CallTarget.Attr.t -> (t, String.t) Result.t
end) =
struct
  type t = { target : Loc.t; attr : Attr.t option }

  let get_target { target; _ } = target

  let eval s c =
    match CallTarget.to_either c with
    | Left (l, a) ->
        let* attr = Attr.eval s a in
        { target = l; attr = Some attr } |> Result.ok
    | Right v ->
        let* v = Store.eval_vn s v in
        let* l = Value.try_loc v in
        { target = l; attr = None } |> Result.ok
end
