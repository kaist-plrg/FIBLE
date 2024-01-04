let translate_jmp (j : L1.Jmp.t_full)
    (alist : (L1.Func.t * L1.SPFA.Immutable.t) List.t)
    (ga : L1.SPFA.Immutable.t) (la : L1.AbsState.t) : L2.Jmp.t_full =
  let njmp : L2.Jmp.t =
    match j.jmp with
    | Junimplemented -> Junimplemented
    | Jfallthrough l -> Jfallthrough l
    | Jjump l -> Jjump l
    | Jjump_ind (vn, ls, _) -> Jjump_ind (vn, ls)
    | Jcbranch (vn, lt, lf) -> Jcbranch (vn, lt, lf)
    | Jcall (l, lret) -> (
        let x =
          List.find_opt (fun ((f, _) : L1.Func.t * _) -> f.entry = l) alist
          |> Option.map (fun ((_, a) : _ * L1.SPFA.Immutable.t) -> a.accesses)
        in
        match x with
        | Some (Fin s) -> Jcall (L1.AccessD.FinSet.max_elt s, 8L, l, lret)
        | _ -> Jcall (0L, 8L, l, lret))
    | Jcall_ind (vn, lret) -> Jcall_ind (0L, 8L, vn, lret)
    | Jtailcall l -> (
        let x =
          List.find_opt (fun ((f, _) : L1.Func.t * _) -> f.entry = l) alist
          |> Option.map (fun ((_, a) : _ * L1.SPFA.Immutable.t) -> a.accesses)
        in
        match x with
        | Some (Fin s) -> Jtailcall (L1.AccessD.FinSet.max_elt s, 8L, l)
        | _ -> Jtailcall (0L, 8L, l))
    | Jtailcall_ind vn -> Jtailcall_ind (0L, 8L, vn)
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
        match o with
        | Register r -> (
            match L1.AbsState.find_opt r.id la with
            | Some { have_sp = Flat true; offset = Flat c } ->
                Isstore ({ value = c; width = 8l }, s)
            | _ -> Istore (d, o, s))
        | _ -> Istore (d, o, s))
  in
  { ins = nins; loc = i.loc; mnem = i.mnem }

let translate_block (b : L1.Block.t)
    (alist : (L1.Func.t * L1.SPFA.Immutable.t) List.t)
    (ga : L1.SPFA.Immutable.t) : L2.Block.t =
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
  {
    fLoc = b.fLoc;
    loc = b.loc;
    body;
    jmp = translate_jmp b.jmp alist ga final_a;
  }

let translate_func (f : L1.Func.t)
    (alist : (L1.Func.t * L1.SPFA.Immutable.t) List.t) (a : L1.SPFA.Immutable.t)
    : L2.Func.t =
  {
    nameo = f.nameo;
    entry = f.entry;
    blocks = List.map (fun b -> translate_block b alist a) f.blocks;
    boundaries = f.boundaries;
    sp_diff = 8L;
    sp_boundary =
      (match a.accesses with
      | Fin s -> (L1.AccessD.FinSet.min_elt s, L1.AccessD.FinSet.max_elt s)
      | _ ->
          [%log
            raise
              (Failure
                 "SPFA.Immutable.analyze returned non-constant sp boundary")]);
  }

let translate_prog (p1 : L1.Prog.t) (sp_num : Int32.t) : L2.Prog.t =
  let ares =
    List.map (fun f -> (f, L1.SPFA.Immutable.analyze f sp_num)) p1.funcs
  in
  let funcs = List.map (fun (f, r) -> translate_func f ares r) ares in
  { sp_num; funcs; rom = p1.rom; externs = p1.externs }

let translate_prog_from_spfa (p1 : L1.Prog.t)
    (spfa_res : (L1.Func.t * L1.SPFA.Immutable.t) list) (sp_num : Int32.t) :
    L2.Prog.t =
  let funcs = List.map (fun (f, a) -> translate_func f spfa_res a) spfa_res in
  { sp_num; funcs; rom = p1.rom; externs = p1.externs }
