open StdlibExt
open Notation
open Basic

module Make (Inst : sig
  type t_full

  val pp_full : Format.formatter -> t_full -> unit
  val get_loc : t_full -> Loc.t
  val is_nop_full : t_full -> bool
end) (Jmp : sig
  type t_full

  val pp_full : Format.formatter -> t_full -> unit
  val get_loc : t_full -> Loc.t
end) (Block : sig
  type t

  val get_body : t -> Inst.t_full list
  val get_jmp : t -> Jmp.t_full
end) (Func : sig
  type t

  val get_entry : t -> Loc.t
  val get_bb : t -> Loc.t -> Block.t option
end) (Prog : sig
  type t

  val get_func_opt : t -> Loc.t -> Func.t option
end) =
struct
  type t = { remaining : Inst.t_full list; jmp : Jmp.t_full }

  let of_block (b : Block.t) : t =
    { remaining = Block.get_body b; jmp = Block.get_jmp b }

  let of_block_loc (p : Prog.t) (floc : Loc.t) (bloc : Loc.t) :
      (t, String.t) Result.t =
    let* f =
      Prog.get_func_opt p floc
      |> Option.to_result
           ~none:(Format.asprintf "Function not found: %a" Loc.pp floc)
    in
    let* b =
      Func.get_bb f bloc
      |> Option.to_result
           ~none:
             (Format.asprintf "Block not found: %a-%a" Loc.pp floc Loc.pp bloc)
    in
    Ok (of_block b)

  let of_func_entry_loc (p : Prog.t) (floc : Loc.t) : (t, String.t) Result.t =
    let* f =
      Prog.get_func_opt p floc
      |> Option.to_result
           ~none:(Format.asprintf "Function not found: %a" Loc.pp floc)
    in
    let* b =
      Func.get_bb f (Func.get_entry f)
      |> Option.to_result
           ~none:
             (Format.asprintf "Block not found: %a-%a" Loc.pp floc Loc.pp
                (Func.get_entry f))
    in
    Ok (of_block b)

  let pp fmt (v : t) =
    match v.remaining with
    | [] -> Format.fprintf fmt "Jmp %a" Jmp.pp_full v.jmp
    | ins :: [] ->
        if Inst.is_nop_full ins then
          Format.fprintf fmt "Jmp %a" Jmp.pp_full v.jmp
        else Format.fprintf fmt "Inst %a" Inst.pp_full ins
    | i :: _ -> Format.fprintf fmt "Inst %a" Inst.pp_full i

  let get_loc (v : t) : Loc.t =
    match v.remaining with [] -> Jmp.get_loc v.jmp | i :: _ -> Inst.get_loc i
end
