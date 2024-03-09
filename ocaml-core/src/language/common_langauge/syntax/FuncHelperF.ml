open StdlibExt
open Basic

module Make (Jmp : sig
  type t_full

  val get_call_target_full : t_full -> Loc.t option
  val is_ret_full : t_full -> bool
end) (Block : sig
  type t

  val get_loc : t -> Loc.t
  val get_jmp : t -> Jmp.t_full
  val succ : t -> Loc.t list
end) (Func : sig
  type t

  val get_blocks : t -> Block.t list
end) =
struct
  let get_bb (f : Func.t) (loc : Loc.t) : Block.t option =
    List.find_opt
      (fun (b : Block.t) -> compare (Block.get_loc b) loc = 0)
      (Func.get_blocks f)

  let get_preds (f : Func.t) (b : Block.t) : Block.t list =
    List.filter
      (fun (b' : Block.t) -> List.mem (Block.get_loc b) (Block.succ b'))
      (Func.get_blocks f)

  let get_ret_blocks (f : Func.t) : Block.t list =
    List.filter
      (fun (b : Block.t) -> Jmp.is_ret_full (Block.get_jmp b))
      (Func.get_blocks f)

  let get_call_targets (f : Func.t) : Loc.t list =
    List.fold_left
      (fun acc (b : Block.t) ->
        match Jmp.get_call_target_full (Block.get_jmp b) with
        | Some target -> target :: acc
        | _ -> acc)
      [] (Func.get_blocks f)
end
