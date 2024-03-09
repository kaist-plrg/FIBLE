open StdlibExt
open Basic

module Make (Jmp : sig
  type t

  val pp : Format.formatter -> t -> unit
  val succ : t -> Loc.t List.t
  val is_ret : t -> bool
  val get_call_target : t -> Loc.t option
end) =
struct
  type t_full = { jmp : Jmp.t; loc : Loc.t; mnem : Mnemonic.t }

  let pp_full (fmt : Format.formatter) (a : t_full) =
    Format.fprintf fmt "%a: %a [%a]" Loc.pp a.loc Jmp.pp a.jmp Mnemonic.pp
      a.mnem

  let succ_full (jmp : t_full) = Jmp.succ jmp.jmp
  let is_ret_full (jmp : t_full) = Jmp.is_ret jmp.jmp
  let get_call_target_full (jmp : t_full) = Jmp.get_call_target jmp.jmp
  let get_loc (p : t_full) = p.loc
end
