let translate_jmp (j : L2Partial.Jmp.t_full) (la : L2Partial.AbsState.t) :
    Jmp.t_full =
  let njmp : Jmp.t =
    match j.jmp with
    | JI j -> JI j
    | JC j -> JC j
    | JT j -> JT j
    | JR { attr = vn } -> JR { attr = () }
  in

  { jmp = njmp; loc = j.loc; mnem = j.mnem }

let translate_inst (i : L2Partial.Inst.t_full) (la : L2Partial.AbsState.t) :
    Inst.t_full =
  i

let translate_block (b : L2Partial.Block.t) (ga : L2Partial.CSA.Immutable.t) :
    Block.t =
  let astate = L1.FSAbsD.AbsLocMapD.find_opt b.loc ga.pre_state in
  let body, final_a =
    match astate with
    | Some v ->
        List.fold_left
          (fun (acci, a) i ->
            ( acci @ [ translate_inst i a ],
              L2Partial.AbsState.post_single_instr i.ins a ))
          ([], v) b.body
    | None ->
        ( List.map (fun i -> translate_inst i L2Partial.AbsState.top) b.body,
          L2Partial.AbsState.top )
  in
  { fLoc = b.fLoc; loc = b.loc; body; jmp = translate_jmp b.jmp final_a }

let translate_func (f : L2Partial.Func.t) (a : L2Partial.CSA.Immutable.t) :
    Func.t =
  {
    nameo = f.nameo;
    entry = f.entry;
    blocks = List.map (fun b -> translate_block b a) f.blocks;
    boundaries = f.boundaries;
    sp_diff = f.sp_diff;
    sp_boundary = f.sp_boundary;
  }

let translate_prog (p1 : L2Partial.Prog.t) : Prog.t =
  let funcs =
    List.map
      (fun f -> translate_func f (L2Partial.CSA.Immutable.analyze f))
      p1.funcs
  in
  {
    sp_num = p1.sp_num;
    funcs;
    rom = p1.rom;
    rspec = p1.rspec;
    externs = p1.externs;
  }

let translate_prog_from_csa (p1 : L2Partial.Prog.t)
    (csa_res : (L2Partial.Func.t * L2Partial.CSA.Immutable.t) list) : Prog.t =
  let funcs = List.map (fun (f, a) -> translate_func f a) csa_res in
  {
    sp_num = p1.sp_num;
    funcs;
    rom = p1.rom;
    rspec = p1.rspec;
    externs = p1.externs;
  }
