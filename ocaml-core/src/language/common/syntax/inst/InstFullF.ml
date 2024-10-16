type 'inst_t poly_t_full = { ins : 'inst_t; loc : Loc.t; mnem : Mnemonic.t }
[@@deriving sexp]

module type S = sig
  module Inst : InstF.S

  type t_full = Inst.t poly_t_full [@@deriving sexp]

  val pp_full : Format.formatter -> t_full -> unit
  val get_loc : t_full -> Loc.t
  val is_nop_full : t_full -> bool
end

module Make (Inst : sig
  type t

  val pp : Format.formatter -> t -> unit
  val is_nop : t -> bool
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end) =
struct
  module Inst = Inst

  type t_full = Inst.t poly_t_full [@@deriving sexp]

  let pp_full (fmt : Format.formatter) (p : t_full) =
    Format.fprintf fmt "%a: %a [%a]" Loc.pp p.loc Inst.pp p.ins Mnemonic.pp
      p.mnem

  let get_loc (p : t_full) = p.loc
  let is_nop_full (p : t_full) = Inst.is_nop p.ins
end
