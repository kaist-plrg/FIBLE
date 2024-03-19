open StdlibExt.Notation

module Make (JRet : sig
  type t

  module Attr : sig
    type t
  end

  val get_attr : t -> Attr.t
end) (Value : sig
  type t

  val try_loc : t -> (Loc.t, String.t) Result.t
end) (Store : sig
  type t

  val eval_vn : t -> VarNode.t -> (Value.t, String.t) Result.t
end) (Attr : sig
  type t

  val eval : Store.t -> JRet.Attr.t -> (t, String.t) Result.t
end) =
struct
  type t = { attr : Attr.t }

  let get_attr { attr; _ } = attr

  let eval (s : Store.t) (j : JRet.t) =
    let* attr = Attr.eval s (JRet.get_attr j) in
    { attr } |> Result.ok
end
