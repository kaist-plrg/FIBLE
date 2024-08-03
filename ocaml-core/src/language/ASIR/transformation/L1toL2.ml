open Syn

let translate_jmp (j : FGIR.Syn.Jmp.t_full)
    (alist : (FGIR.Syn.Func.t * FGIR.SPFA.Immutable.t) List.t)
    (ga : FGIR.SPFA.Immutable.t) (la : FGIR.AbsState.t) : Jmp.t_full =
  let njmp : Jmp.t =
    match j.jmp with
    | JI v -> JI v
    | JR v -> JR { attr = () }
    | JC { target = Cdirect { target; attr }; fallthrough } ->
        let x =
          List.find_opt
            (fun ((f, _) : FGIR.Syn.Func.t * _) -> f.entry = target)
            alist
          |> Option.map (fun ((_, a) : _ * FGIR.SPFA.Immutable.t) -> a.accesses)
        in
        JC
          {
            target = Cdirect { target; attr };
            fallthrough;
            attr =
              {
                reserved_stack =
                  (match x with
                  | Some (Fin s) -> FGIR.AccessD.FinSet.max_elt s
                  | _ -> 0L);
                sp_diff = 8L;
              };
          }
    | JC { target = Cind { target; _ }; fallthrough } ->
        JC
          {
            target = Cind { target };
            fallthrough;
            attr = { reserved_stack = 0L; sp_diff = 8L };
          }
    | JT { target = Cdirect { target; attr } } ->
        let x =
          List.find_opt
            (fun ((f, _) : FGIR.Syn.Func.t * _) -> f.entry = target)
            alist
          |> Option.map (fun ((_, a) : _ * FGIR.SPFA.Immutable.t) -> a.accesses)
        in
        JT
          {
            target = Cdirect { target; attr };
            attr =
              {
                reserved_stack =
                  (match x with
                  | Some (Fin s) ->
                      (Int64.mul
                         (Int64.div
                            (Int64.add (FGIR.AccessD.FinSet.max_elt s) 7L)
                            8L))
                        8L
                  | _ -> 0L);
                sp_diff = 8L;
              };
          }
    | JT { target = Cind { target } } ->
        JT
          {
            target = Cind { target };
            attr = { reserved_stack = 0L; sp_diff = 8L };
          }
  in

  { jmp = njmp; loc = j.loc; mnem = j.mnem }

let translate_inst (i : FGIR.Syn.Inst.t_full) (ga : FGIR.SPFA.Immutable.t)
    (la : FGIR.AbsState.t) : Inst.t_full =
  let nins : Inst.t =
    (FGIR.Syn.Inst.fold : _ -> _ -> _ -> _ -> _ -> Inst.t)
      (function
        | Load { space; pointer; output } -> (
            match pointer with
            | Register r -> (
                match FGIR.ARegFile.get la.regs r.id with
                | Flat (SP c) -> Second (Sload { offset = c; output })
                | _ -> First (Load { space; pointer; output }))
            | _ -> First (Load { space; pointer; output }))
        | Store { space; pointer; value } -> (
            match pointer with
            | Register r -> (
                match FGIR.ARegFile.get la.regs r.id with
                | Flat (SP c) -> Second (Sstore { offset = c; value })
                | _ -> First (Store { space; pointer; value }))
            | _ -> First (Store { space; pointer; value })))
      (fun v -> Third v)
      (fun v -> Fourth v)
      (fun v -> Fifth v)
      i.ins
  in
  { ins = nins; loc = i.loc; mnem = i.mnem }

let translate_block (b : FGIR.Syn.Block.t)
    (alist : (FGIR.Syn.Func.t * FGIR.SPFA.Immutable.t) List.t)
    (ga : FGIR.SPFA.Immutable.t) : Block.t =
  let astate = FGIR.FSAbsD.AbsLocMapD.find_opt b.loc ga.states.pre_state in
  let body, final_a =
    match astate with
    | Some v ->
        List.fold_left
          (fun (acci, a) i ->
            ( acci @ [ translate_inst i ga a ],
              snd (FGIR.AbsState.post_single_instr i.ins a) ))
          ([], v) b.body
    | None ->
        ( List.map (fun i -> translate_inst i ga FGIR.AbsState.top) b.body,
          FGIR.AbsState.top )
  in
  {
    fLoc = b.fLoc;
    loc = b.loc;
    body;
    jmp = translate_jmp b.jmp alist ga final_a;
  }

let translate_func (f : FGIR.Syn.Func.t)
    (alist : (FGIR.Syn.Func.t * FGIR.SPFA.Immutable.t) List.t)
    (a : FGIR.SPFA.Immutable.t) : Func.t =
  {
    nameo = f.nameo;
    entry = f.entry;
    blocks = List.map (fun b -> translate_block b alist a) f.blocks;
    boundaries = f.boundaries;
    attr =
      {
        sp_diff = 8L;
        sp_boundary =
          (match a.accesses with
          | Fin s ->
              ( FGIR.AccessD.FinSet.min_elt s,
                (Int64.mul
                   (Int64.div (Int64.add (FGIR.AccessD.FinSet.max_elt s) 7L) 8L))
                  8L )
          | _ ->
              [%log
                raise
                  (Failure
                     "SPFA.Immutable.analyze returned non-constant sp boundary")]);
      };
  }

let translate_prog (p1 : FGIR.Syn.Prog.t) (sp_num : Int32.t) (fp_num : Int32.t)
    : Prog.t =
  let ares =
    List.map
      (fun f -> (f, FGIR.SPFA.Immutable.analyze f sp_num fp_num))
      p1.funcs
  in
  let funcs = List.map (fun (f, r) -> translate_func f ares r) ares in
  {
    sp_num;
    fp_num;
    funcs;
    rom = p1.rom;
    rspec = p1.rspec;
    externs = p1.externs;
    objects = p1.objects;
  }

let translate_prog_from_spfa (p1 : FGIR.Syn.Prog.t)
    (spfa_res : (FGIR.Syn.Func.t * FGIR.SPFA.Immutable.t) list)
    (sp_num : Int32.t) (fp_num : Int32.t) : Prog.t =
  let funcs = List.map (fun (f, a) -> translate_func f spfa_res a) spfa_res in
  {
    sp_num;
    fp_num;
    funcs;
    rom = p1.rom;
    rspec = p1.rspec;
    externs = p1.externs;
    objects = p1.objects;
  }
