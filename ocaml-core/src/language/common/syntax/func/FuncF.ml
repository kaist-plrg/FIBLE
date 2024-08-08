module type S = sig
  module Jmp : JmpFullF.S
  module Block : BlockF.S

  module Attr : sig
    type t

    val pp : Format.formatter -> t -> unit
    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
  end

  type t

  val pp : Format.formatter -> t -> unit
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val get_bb : t -> Loc.t -> Block.t option
  val get_preds : t -> Block.t -> Block.t list
  val get_ret_blocks : t -> Block.t list
  val get_call_targets : t -> Loc.t list
  val nameo : t -> String.t Option.t
  val entry : t -> Loc.t
  val boundaries : t -> LocSet.t
  val blocks : t -> Block.t List.t
  val attr : t -> Attr.t
end

module Make
    (Jmp : JmpFullF.S)
    (Block : BlockF.S with module Jmp = Jmp)
    (Attr : sig
      type t

      val pp : Format.formatter -> t -> unit
      val t_of_sexp : Sexplib.Sexp.t -> t
      val sexp_of_t : t -> Sexplib.Sexp.t
    end) =
struct
  module Jmp = Jmp
  module Block = Block
  module Attr = Attr

  type t = {
    nameo : String.t Option.t;
    entry : Loc.t;
    boundaries : LocSet.t;
    blocks : Block.t List.t;
    attr : Attr.t;
  }
  [@@deriving sexp, show, fields]

  let get_bb (f : t) (loc : Loc.t) : Block.t option =
    List.find_opt
      (fun (b : Block.t) -> compare (Block.get_loc b) loc = 0)
      (blocks f)

  let get_preds (f : t) (b : Block.t) : Block.t list =
    List.filter
      (fun (b' : Block.t) -> List.mem (Block.get_loc b) (Block.succ b'))
      (blocks f)

  let get_ret_blocks (f : t) : Block.t list =
    List.filter
      (fun (b : Block.t) -> Jmp.is_ret_full (Block.get_jmp b))
      (blocks f)

  let get_call_targets (f : t) : Loc.t list =
    List.fold_left
      (fun acc (b : Block.t) ->
        match Jmp.get_call_target_full (Block.get_jmp b) with
        | Some target -> target :: acc
        | _ -> acc)
      [] (blocks f)
end
