let translate_jmp (j : L2.Jmp.t_full) (la : L2.AbsState.t) : L3.Jmp.t_full =
  let njmp : L3.Jmp.t =
    match j.jmp with
    | Junimplemented -> Junimplemented
    | Jfallthrough l -> Jfallthrough l
    | Jjump l -> Jjump l
    | Jjump_ind (vn, ls) -> Jjump_ind (vn, ls)
    | Jcbranch (vn, lt, lf) -> Jcbranch (vn, lt, lf)
    | Jcall (spdiff, l, lret) -> Jcall (spdiff, l, lret)
    | Jcall_ind (spdiff, vn, lret) -> Jcall_ind (spdiff, vn, lret)
    | Jret vn -> Jret
  in

  { jmp = njmp; loc = j.loc; mnem = j.mnem }

let translate_inst (i : L2.Inst.t_full) (la : L2.AbsState.t) : L3.Inst.t_full =
  let nins : L3.Inst.t =
    match i.ins with
    | INop -> INop
    | Iassignment (d, s) -> Iassignment (d, s)
    | Iload (d, s, o) -> Iload (d, s, o)
    | Istore (d, o, s) -> Istore (d, o, s)
    | Isload (offset, o) ->
        if offset.value > 0L then Ipload (offset, o) else Ilload (offset, o)
    | Isstore (offset, o) ->
        if offset.value > 0L then Ilstore (offset, o) else Ilstore (offset, o)
  in
  { ins = nins; loc = i.loc; mnem = i.mnem }

let translate_block (b : L2.Block.t) (ga : L2.CSA.Immutable.t) : L3.Block.t =
  let astate = L1.FSAbsD.AbsLocMapD.find_opt b.loc ga.pre_state in
  let body, final_a =
    match astate with
    | Some v ->
        List.fold_left
          (fun (acci, a) i ->
            ( acci @ [ translate_inst i a ],
              L2.AbsState.post_single_instr i.ins a ))
          ([], v) b.body
    | None ->
        ( List.map (fun i -> translate_inst i L2.AbsState.top) b.body,
          L2.AbsState.top )
  in
  { loc = b.loc; body; jmp = translate_jmp b.jmp final_a }

let translate_func (f : L2.Func.t) (a : L2.CSA.Immutable.t) : L3.Func.t =
  {
    nameo = f.nameo;
    entry = f.entry;
    blocks = List.map (fun b -> translate_block b a) f.blocks;
    boundaries = f.boundaries;
    sp_diff = f.sp_diff;
    sp_boundary = f.sp_boundary;
  }

let translate_prog (p1 : L2.Prog.t) : L3.Prog.t =
  let funcs =
    List.map (fun f -> translate_func f (L2.CSA.Immutable.analyze f)) p1.funcs
  in
  { sp_num = p1.sp_num; funcs; rom = p1.rom; externs = p1.externs }

let translate_prog_from_csa (p1 : L2.Prog.t)
    (csa_res : (L2.Func.t * L2.CSA.Immutable.t) list) : L3.Prog.t =
  let funcs = List.map (fun (f, a) -> translate_func f a) csa_res in
  { sp_num = p1.sp_num; funcs; rom = p1.rom; externs = p1.externs }
