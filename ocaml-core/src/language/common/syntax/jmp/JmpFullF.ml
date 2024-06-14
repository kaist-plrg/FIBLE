open StdlibExt

module type JumpSig = sig
  type t

  val pp : Format.formatter -> t -> unit
  val succ : t -> Loc.t List.t
  val is_ret : t -> bool
  val resolve_calltarget_opt : t -> Loc.t option
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

module Make (Jmp : JumpSig) = struct
  type t_full = { jmp : Jmp.t; loc : Loc.t; mnem : Mnemonic.t }
  [@@deriving sexp]

  let pp_full (fmt : Format.formatter) (a : t_full) =
    Format.fprintf fmt "%a: %a [%a]" Loc.pp a.loc Jmp.pp a.jmp Mnemonic.pp
      a.mnem

  let succ_full (jmp : t_full) = Jmp.succ jmp.jmp
  let is_ret_full (jmp : t_full) = Jmp.is_ret jmp.jmp
  let get_call_target_full (jmp : t_full) = Jmp.resolve_calltarget_opt jmp.jmp
  let get_loc (p : t_full) = p.loc
end

module MakeFromJmps
    (JIntra : JumpSig)
    (JCall : JumpSig)
    (JTailCall : JumpSig)
    (JRet : JumpSig) =
struct
  module Inner = struct
    type t = JI of JIntra.t | JC of JCall.t | JT of JTailCall.t | JR of JRet.t
    [@@deriving sexp]

    let pp fmt (a : t) =
      match a with
      | JI j -> JIntra.pp fmt j
      | JC j -> JCall.pp fmt j
      | JT j -> JTailCall.pp fmt j
      | JR j -> JRet.pp fmt j

    let succ jmp =
      match jmp with
      | JI j -> JIntra.succ j
      | JC j -> JCall.succ j
      | JT j -> JTailCall.succ j
      | JR j -> JRet.succ j

    let is_ret jmp =
      match jmp with
      | JI j -> JIntra.is_ret j
      | JC j -> JCall.is_ret j
      | JT j -> JTailCall.is_ret j
      | JR j -> JRet.is_ret j

    let resolve_calltarget_opt (j : t) : Loc.t option =
      match j with
      | JI j -> JIntra.resolve_calltarget_opt j
      | JC j -> JCall.resolve_calltarget_opt j
      | JT j -> JTailCall.resolve_calltarget_opt j
      | JR j -> JRet.resolve_calltarget_opt j
  end

  include Inner
  include Make (Inner)
end
