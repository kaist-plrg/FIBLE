open StdlibExt
open Basic

module Make (Inst : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (Jmp : sig
  type t

  val succ : t -> Loc.t list
  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = { loc : Loc.t; body : Inst.t list; jmp : Jmp.t }

  let fold_left f acc { body; jmp; _ } = List.fold_left f acc body

  let pp fmt { loc; body; jmp } =
    Format.fprintf fmt "@[<v 2>block %a@ %a@ %a@]" Loc.pp loc
      (Format.pp_print_list Inst.pp)
      body Jmp.pp jmp

  let succ { jmp; _ } = Jmp.succ jmp
end
