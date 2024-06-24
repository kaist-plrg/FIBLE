open Sexplib.Std

module Make (Inst : sig
  type t_full

  val pp_full : Format.formatter -> t_full -> unit
  val t_full_of_sexp : Sexplib.Sexp.t -> t_full
  val sexp_of_t_full : t_full -> Sexplib.Sexp.t
end) (Jmp : sig
  type t_full

  val succ_full : t_full -> Loc.t list
  val pp_full : Format.formatter -> t_full -> unit
  val t_full_of_sexp : Sexplib.Sexp.t -> t_full
  val sexp_of_t_full : t_full -> Sexplib.Sexp.t
end) =
struct
  type t = {
    fLoc : Loc.t;
    loc : Loc.t;
    body : Inst.t_full list;
    jmp : Jmp.t_full;
  }
  [@@deriving sexp]

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
