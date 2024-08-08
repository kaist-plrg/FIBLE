open Sexplib.Std

module type S = sig
  type t

  module Inst : InstFullF.S
  module Jmp : JmpFullF.S

  val pp : Format.formatter -> t -> unit
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val succ : t -> Loc.t list
  val fold_left : ('a -> Inst.t_full -> 'a) -> 'a -> t -> 'a
  val get_fLoc : t -> Loc.t
  val get_loc : t -> Loc.t
  val get_body : t -> Inst.t_full list
  val get_jmp : t -> Jmp.t_full
end

module Make (Inst : InstFullF.S) (Jmp : JmpFullF.S) = struct
  type t = {
    fLoc : Loc.t;
    loc : Loc.t;
    body : Inst.t_full list;
    jmp : Jmp.t_full;
  }
  [@@deriving sexp]

  module Inst = Inst
  module Jmp = Jmp

  let fold_left f acc { body; jmp; _ } = List.fold_left f acc body

  let pp fmt { loc; body; jmp } =
    Format.fprintf fmt "@[<v 2>block %a@ %a@ %a@]" Loc.pp loc
      (Format.pp_print_list Inst.pp_full)
      body Jmp.pp_full jmp

  let succ { jmp; _ } = Jmp.succ_full jmp
  let get_fLoc { fLoc; _ } = fLoc
  let get_loc { loc; _ } = loc
  let get_body { body; _ } = body
  let get_jmp { jmp; _ } = jmp
end
