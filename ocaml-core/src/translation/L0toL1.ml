open Basic
open Basic_domain

let translate_stmt (loc : Loc.t) (i : L0.Inst.t_full) : L1.Inst.t_full =
  match i.ins with
  | Iassignment (x, e) -> { loc; ins = Iassignment (x, e); mnem = i.mnem }
  | Iload (i0, i1, o) -> { loc; ins = Iload (i0, i1, o); mnem = i.mnem }
  | Istore (i0, i1, i2) -> { loc; ins = Istore (i0, i1, i2); mnem = i.mnem }
  | _ -> { loc; ins = INop; mnem = i.mnem }

let translate_jmp (loc : Loc.t) (i : L0.Inst.t_full) (next : Loc.t)
    (jmps : LocSetD.t) : L1.Jmp.t_full =
  match i.ins with
  | Ijump l ->
      {
        loc;
        jmp =
          (if String.starts_with ~prefix:"J" i.mnem then Jjump l
           else Jcall (l, next));
        mnem = i.mnem;
      }
  | Ijump_ind vn ->
      {
        loc;
        jmp =
          (if String.equal i.mnem "RET" then Jret vn
           else if String.equal i.mnem "CALL" then Jcall_ind (vn, next)
           else Jjump_ind (vn, jmps));
        mnem = i.mnem;
      }
  | Icbranch (a, l0) -> { loc; jmp = Jcbranch (a, l0, next); mnem = i.mnem }
  | Iunimplemented -> { loc; jmp = Junimplemented; mnem = i.mnem }
  | _ -> { loc; jmp = Jfallthrough next; mnem = i.mnem }

let translate_block (p0 : L0.Prog.t) (entry : Loc.t) (cf : L0.CFA.Immutable.t)
    (entries : LocSetD.t) : L1.Block.t =
  let rec aux (loc : Loc.t) (acc : L1.Inst.t_full list) : L1.Block.t =
    let ninst = L0.Prog.get_ins_full p0 loc in
    match ninst with
    | None ->
        {
          loc = entry;
          body = List.rev acc;
          jmp = { loc; jmp = Junimplemented; mnem = "" };
        }
    | Some i -> (
        match L0.JumpD.find_opt loc cf.sound_jump with
        | None ->
            {
              loc = entry;
              body = List.rev (translate_stmt loc i :: acc);
              jmp = { loc; jmp = Junimplemented; mnem = i.mnem };
            }
        | Some jmps ->
            if LocSetD.cardinal jmps = 1 then
              let jmp = LocSetD.choose jmps in
              if LocSetD.mem jmp entries then
                {
                  loc = entry;
                  body = List.rev (translate_stmt loc i :: acc);
                  jmp = translate_jmp loc i jmp jmps;
                }
              else aux jmp (translate_stmt loc i :: acc)
            else
              {
                loc = entry;
                body = List.rev (translate_stmt loc i :: acc);
                jmp = translate_jmp loc i (L0.Prog.fallthru p0 loc) jmps;
              })
  in
  aux entry []

let translate_func (p0 : L0.Prog.t) (nameo : String.t option) (entry : Addr.t)
    (cf : L0.CFA.Immutable.t) : L1.Func.t =
  let boundary_entries = fst cf.analysis_contour.boundary_point in
  let other_block_entires =
    L0.JumpD.to_seq cf.sound_jump
    |> Seq.filter_map (fun (a, _) ->
           let preds = L0.JumpD.get_preds cf.sound_jump a in
           match preds with
           | [ p ] ->
               if LocSetD.cardinal (L0.JumpD.find p cf.sound_jump) >= 2 then Some a
               else None
           | _ -> Some a)
  in
  let entries = LocSetD.add_seq other_block_entires boundary_entries in
  let blocks =
    LocSetD.to_seq entries
    |> Seq.map (fun e -> translate_block p0 e cf entries)
    |> List.of_seq
  in
  { nameo; entry = (entry, 0); boundaries = boundary_entries; blocks }

let translate_prog (p0 : L0.Prog.t) (entries : Addr.t list) : L1.Prog.t =
  let funcs =
    List.map
      (fun e -> translate_func p0 None e (L0.CFA.Immutable.follow_flow p0 e))
      entries
  in
  { funcs; rom = p0.rom }

let translate_prog_from_cfa (p0 : L0.Prog.t)
    (cfa_res : (String.t * Addr.t * L0.CFA.Immutable.t) list) : L1.Prog.t =
  let funcs =
    List.map (fun (fname, e, cf) -> translate_func p0 (Some fname) e cf) cfa_res
  in
  { funcs; rom = p0.rom }
