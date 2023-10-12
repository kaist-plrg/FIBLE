open Basic
open Basic_domain

let translate_stmt (i0 : L0.Inst.t) : L1.Inst.t =
  match i0 with
  | Iassignment (x, e) -> Iassignment (x, e)
  | Iload (i0, i1, o) -> Iload (i0, i1, o)
  | Istore (i0, i1, i2) -> Istore (i0, i1, i2)
  | _ -> INop

let translate_jmp (i0 : L0.Inst.t) (next : Loc.t) (jmps : LocSetD.t) : L1.Jmp.t
    =
  match i0 with
  | Ijump (Jbranch, l) -> Jjump l
  | Ijump (Jcall, l) -> Jcall (l, next)
  | Ijump_ind (JIbranch, vn) -> Jjump_ind (vn, jmps)
  | Ijump_ind (JIcall, vn) -> Jcall_ind (vn, next)
  | Ijump_ind (JIret, vn) -> Jret vn
  | Icbranch (a, l0) -> Jcbranch (a, l0, next)
  | Iunimplemented -> Junimplemented
  | _ -> Jfallthrough next

let translate_block (p0 : L0.Prog.t) (entry : Loc.t) (cf : CFA.Immutable.t)
    (entries : LocSetD.t) : L1.Block.t =
  let rec aux (loc : Loc.t) (acc : L1.Inst.t list) : L1.Block.t =
    let ninst = L0.Prog.get_ins p0 loc in
    match ninst with
    | None -> { loc = entry; body = List.rev acc; jmp = Junimplemented }
    | Some i -> (
        match JumpD.find_opt loc cf.sound_jump with
        | None ->
            {
              loc = entry;
              body = List.rev (translate_stmt i :: acc);
              jmp = Junimplemented;
            }
        | Some jmps ->
            if LocSetD.cardinal jmps = 1 then
              let jmp = LocSetD.choose jmps in
              if LocSetD.mem jmp entries then
                {
                  loc = entry;
                  body = List.rev (translate_stmt i :: acc);
                  jmp = translate_jmp i jmp jmps;
                }
              else aux jmp (translate_stmt i :: acc)
            else
              {
                loc = entry;
                body = List.rev (translate_stmt i :: acc);
                jmp = translate_jmp i (L0.Prog.fallthru p0 loc) jmps;
              })
  in
  aux entry []

let translate_func (p0 : L0.Prog.t) (nameo: String.t option) (entry : Addr.t) (cf : CFA.Immutable.t) :
    L1.Func.t =
  let boundary_entries = fst cf.analysis_contour.boundary_point in
  let other_block_entires =
    JumpD.to_seq cf.sound_jump
    |> Seq.filter_map (fun (a, _) ->
           let preds = JumpD.get_preds cf.sound_jump a in
           match preds with
           | [ p ] ->
               if LocSetD.cardinal (JumpD.find p cf.sound_jump) >= 2 then Some a
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
      (fun e -> translate_func p0 None e (CFA.Immutable.follow_flow p0 e))
      entries
  in
  { funcs; rom = p0.rom }

let translate_prog_from_cfa (p0: L0.Prog.t) (cfa_res: (String.t * Addr.t * CFA.Immutable.t) list) : L1.Prog.t =
  let funcs =
    List.map
      (fun (fname, e, cf) -> translate_func p0 (Some fname) e cf)
      cfa_res
  in
  { funcs; rom = p0.rom }