open Basic
open Basic_domain
open Basic_collection

let translate_stmt (loc : Loc.t) (i : L0.Inst.t_full) : L1Partial.Inst.t_full =
  match i.ins with
  | Iassignment (x, e) -> { loc; ins = Iassignment (x, e); mnem = i.mnem }
  | Iload (i0, i1, o) -> { loc; ins = Iload (i0, i1, o); mnem = i.mnem }
  | Istore (i0, i1, i2) -> { loc; ins = Istore (i0, i1, i2); mnem = i.mnem }
  | _ -> { loc; ins = INop; mnem = i.mnem }

let translate_jmp (p : L0.Prog.t) (loc : Loc.t) (i : L0.Inst.t_full)
    (next : Loc.t) (jmps : Loc.t List.t) : L1Partial.Jmp.t_full =
  match i.ins with
  | Ijump l ->
      {
        loc;
        jmp =
          (if
             (not (AddrMap.mem (Loc.to_addr l) p.externs))
             && (String.starts_with ~prefix:"J" i.mnem
                || String.starts_with ~prefix:"M" i.mnem)
           then Jjump l
           else if L0.Prog.get_ins p l |> Option.is_some then Jcall (l, next)
           else Jtailcall l);
        mnem = i.mnem;
      }
  | Ijump_ind vn ->
      {
        loc;
        jmp =
          (if String.equal i.mnem "RET" then Jret vn
           else if String.equal i.mnem "CALL" then Jcall_ind (vn, next)
           else JswitchStop vn);
        mnem = i.mnem;
      }
  | Icbranch (a, l0) -> { loc; jmp = Jcbranch (a, l0, next); mnem = i.mnem }
  | Iunimplemented -> { loc; jmp = Junimplemented; mnem = i.mnem }
  | _ -> { loc; jmp = Jfallthrough next; mnem = i.mnem }

let translate_block (p0 : L0.Prog.t) (fentry : Loc.t) (entry : Loc.t)
    (cf : L0.Shallow_CFA.t) (entries : LocSetD.t) : L1Partial.Block.t =
  let rec aux (loc : Loc.t) (acc : L1Partial.Inst.t_full list) :
      L1Partial.Block.t =
    let ninst = L0.Prog.get_ins_full p0 loc in
    match ninst with
    | None ->
        {
          fLoc = fentry;
          loc = entry;
          body = List.rev acc;
          jmp = { loc; jmp = Junimplemented; mnem = "" };
        }
    | Some i ->
        if L0.JumpG.G.mem_vertex cf.sound_jump loc then
          match L0.JumpG.G.succ cf.sound_jump loc with
          | [ jmp ] ->
              if LocSetD.mem jmp entries then
                {
                  fLoc = fentry;
                  loc = entry;
                  body = List.rev (translate_stmt loc i :: acc);
                  jmp = translate_jmp p0 loc i jmp [ jmp ];
                }
              else aux jmp (translate_stmt loc i :: acc)
          | jmps ->
              {
                fLoc = fentry;
                loc = entry;
                body = List.rev (translate_stmt loc i :: acc);
                jmp = translate_jmp p0 loc i (L0.Prog.fallthru p0 loc) jmps;
              }
        else
          {
            fLoc = fentry;
            loc = entry;
            body = List.rev (translate_stmt loc i :: acc);
            jmp = { loc; jmp = Junimplemented; mnem = i.mnem };
          }
  in

  aux entry []

let translate_func (p0 : L0.Prog.t) (nameo : String.t option) (entry : Addr.t)
    (cf : L0.Shallow_CFA.t) : L1Partial.Func.t =
  let boundary_entries = fst cf.boundary_point in
  let other_block_entires =
    L0.JumpG.G.fold_vertex
      (fun l s ->
        match L0.JumpG.G.pred cf.sound_jump l with
        | [ p ] ->
            if L0.JumpG.G.out_degree cf.sound_jump p >= 2 then LocSet.add l s
            else s
        | _ -> LocSet.add l s)
      cf.sound_jump LocSet.empty
  in
  let entries = LocSetD.union other_block_entires boundary_entries in
  let blocks =
    LocSetD.to_seq entries
    |> Seq.map (fun e -> translate_block p0 (entry, 0) e cf entries)
    |> List.of_seq
  in
  { nameo; entry = (entry, 0); boundaries = boundary_entries; blocks }

let translate_prog (p0 : L0.Prog.t) (entries : Addr.t list) : L1Partial.Prog.t =
  let funcs =
    List.map
      (fun e -> translate_func p0 None e (L0.Shallow_CFA.follow_flow p0 e))
      entries
  in
  { funcs; rom = p0.rom; externs = p0.externs }

let translate_prog_from_cfa (p0 : L0.Prog.t)
    (cfa_res : (String.t * Addr.t * L0.Shallow_CFA.t) list) : L1Partial.Prog.t =
  let funcs =
    List.map (fun (fname, e, cf) -> translate_func p0 (Some fname) e cf) cfa_res
  in
  { funcs; rom = p0.rom; externs = p0.externs }
