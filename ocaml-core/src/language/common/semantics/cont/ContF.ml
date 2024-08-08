module type S = sig
  module Inst : InstFullF.S
  module Jmp : JmpFullF.S
  module Block : BlockF.S with module Inst = Inst and module Jmp = Jmp
  module Func : FuncF.S with module Jmp = Jmp and module Block = Block

  module Prog : sig
    type t
  end

  type t

  val of_block : Block.t -> t
  val of_loc : Prog.t -> Loc.t -> Loc.t -> (t, String.t) Result.t
  val of_func_entry_loc : Prog.t -> Loc.t -> (t, String.t) Result.t
  val pp : Format.formatter -> t -> unit
  val get_loc : t -> Loc.t
  val remaining : t -> Inst.t_full list
  val jmp : t -> Jmp.t_full
end

module Make
    (Inst : InstFullF.S)
    (Jmp : JmpFullF.S)
    (Block : BlockF.S with module Inst = Inst and module Jmp = Jmp)
    (Func : FuncF.S with module Jmp = Jmp and module Block = Block)
    (Prog : sig
      type t

      val get_func_opt : t -> Loc.t -> Func.t option
    end) =
struct
  module Inst = Inst
  module Jmp = Jmp
  module Block = Block
  module Func = Func
  module Prog = Prog

  type t = { remaining : Inst.t_full list; jmp : Jmp.t_full }
  [@@deriving fields]

  let of_block (b : Block.t) : t =
    { remaining = Block.get_body b; jmp = Block.get_jmp b }

  let of_loc (p : Prog.t) (floc : Loc.t) (bloc : Loc.t) : (t, String.t) Result.t
      =
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
      Func.get_bb f (Func.entry f)
      |> Option.to_result
           ~none:
             (Format.asprintf "Block not found: %a-%a" Loc.pp floc Loc.pp
                (Func.entry f))
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
