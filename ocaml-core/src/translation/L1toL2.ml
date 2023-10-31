let translate_jmp (j : L1.Jmp.t_full) (ga : L1.SPFA.Immutable.t)
    (la : L1.AbsState.t) : L2.Jmp.t_full =
  let njmp : L2.Jmp.t =
    match j.jmp with
    | Junimplemented -> Junimplemented
    | Jfallthrough l -> Jfallthrough l
    | Jjump l -> Jjump l
    | Jjump_ind (vn, ls) -> Jjump_ind (vn, ls)
    | Jcbranch (vn, lt, lf) -> Jcbranch (vn, lt, lf)
    | Jcall (l, lret) -> Jcall (8L, l, lret)
    | Jcall_ind (vn, lret) -> Jcall_ind (8L, vn, lret)
    | Jret vn -> Jret vn
  in

  { jmp = njmp; loc = j.loc; mnem = j.mnem }

let translate_inst (i : L1.Inst.t_full) (ga : L1.SPFA.Immutable.t)
    (la : L1.AbsState.t) : L2.Inst.t_full =
  let nins : L2.Inst.t =
    match i.ins with
    | INop -> INop
    | Iassignment (d, s) -> Iassignment (d, s)
    | Iload (d, s, o) -> (
        match s with
        | Register r -> (
            match L1.AbsState.find_opt r.id la with
            | Some { have_sp = Flat true; offset = Flat c } ->
                Isload ({ value = c; width = 8l }, o)
            | _ -> Iload (d, s, o))
        | _ -> Iload (d, s, o))
    | Istore (d, o, s) -> (
        match s with
        | Register r -> (
            match L1.AbsState.find_opt r.id la with
            | Some { have_sp = Flat true; offset = Flat c } ->
                Isstore ({ value = c; width = 8l }, o)
            | _ -> Istore (d, o, s))
        | _ -> Istore (d, o, s))
  in
  { ins = nins; loc = i.loc; mnem = i.mnem }

let translate_block (b : L1.Block.t) (ga : L1.SPFA.Immutable.t) : L2.Block.t =
  let astate = L1.FSAbsD.AbsLocMapD.find_opt b.loc ga.states.pre_state in
  let body, final_a =
    match astate with
    | Some v ->
        List.fold_left
          (fun (acci, a) i ->
            ( acci @ [ translate_inst i ga a ],
              snd (L1.AbsState.post_single_instr i.ins a) ))
          ([], v) b.body
    | None ->
        ( List.map (fun i -> translate_inst i ga L1.AbsState.top) b.body,
          L1.AbsState.top )
  in
  { loc = b.loc; body; jmp = translate_jmp b.jmp ga final_a }

let translate_func (f : L1.Func.t) (a : L1.SPFA.Immutable.t) : L2.Func.t =
  {
    nameo = f.nameo;
    entry = f.entry;
    blocks = List.map (fun b -> translate_block b a) f.blocks;
    boundaries = f.boundaries;
    sp_diff = 8L;
    sp_boundary =
      (match a.accesses with
      | Fin s -> (L1.AccessD.FinSet.min_elt s, L1.AccessD.FinSet.max_elt s)
      | _ ->
          raise
            (Failure "SPFA.Immutable.analyze returned non-constant sp boundary"));
  }

let translate_prog (p1 : L1.Prog.t) (sp_num : int64) : L2.Prog.t =
  let funcs =
    List.map
      (fun f -> translate_func f (L1.SPFA.Immutable.analyze f sp_num))
      p1.funcs
  in
  { sp_num; funcs; rom = p1.rom; externs = p1.externs }

let translate_prog_from_spfa (p1 : L1.Prog.t)
    (spfa_res : (L1.Func.t * L1.SPFA.Immutable.t) list) (sp_num : int64) :
    L2.Prog.t =
  let funcs = List.map (fun (f, a) -> translate_func f a) spfa_res in
  { sp_num; funcs; rom = p1.rom; externs = p1.externs }
