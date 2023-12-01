open Basic

let ( let* ) = Result.bind

type t = { remaining : Inst.t_full list; jmp : Jmp.t_full }

let of_block (b : Block.t) : t = { remaining = b.body; jmp = b.jmp }

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
    Func.get_bb f f.entry
    |> Option.to_result
         ~none:
           (Format.asprintf "Block not found: %a-%a" Loc.pp floc Loc.pp f.entry)
  in
  Ok (of_block b)

let pp fmt (v : t) =
  match v.remaining with
  | [] -> Format.fprintf fmt "Jmp %a" Jmp.pp_full v.jmp
  | { ins = INop; _ } :: [] -> Format.fprintf fmt "Jmp %a" Jmp.pp_full v.jmp
  | i :: _ -> Format.fprintf fmt "Inst %a" Inst.pp_full i
