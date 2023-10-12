let translate_func (f : L1.Func.t) (a : SPFA.Immutable.t) : L2.Func.t =
  {
    nameo = f.nameo;
    entry = f.entry;
    blocks = f.blocks;
    boundaries = f.boundaries;
    sp_boundary =
      (match a.accesses with
      | Fin s -> AccessD.FinSet.min_elt s, AccessD.FinSet.max_elt s
      | _ ->
          raise
            (Failure "SPFA.Immutable.analyze returned non-constant sp boundary"));
  }

let translate_prog (p1 : L1.Prog.t) (sp_num : int64) : L2.Prog.t =
  let funcs =
    List.map
      (fun f -> translate_func f (SPFA.Immutable.analyze f sp_num))
      p1.funcs
  in
  { sp_num; funcs; rom = p1.rom }

let translate_prog_from_spfa (p1: L1.Prog.t) (spfa_res: (L1.Func.t * SPFA.Immutable.t) list) (sp_num: int64): L2.Prog.t =
  let funcs =
    List.map
      (fun (f, a) -> translate_func f a)
      spfa_res
  in
  { sp_num; funcs; rom = p1.rom }