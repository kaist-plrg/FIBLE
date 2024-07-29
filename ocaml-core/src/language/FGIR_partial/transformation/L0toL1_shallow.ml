open Common
open Basic_domain
open Syn

let translate_stmt (loc : Loc.t) (i : ILIR.Syn.Inst.t_full) : Inst.t_full =
  match i.ins with
  | First x -> { loc; ins = First x; mnem = i.mnem }
  | Second x -> { loc; ins = Second x; mnem = i.mnem }
  | _ -> { loc; ins = Third INop; mnem = i.mnem }

let translate_jmp (p : ILIR.Syn.Prog.t) (loc : Loc.t) (i : ILIR.Syn.Inst.t_full)
    (next : Loc.t) (jmps : Loc.t List.t) (known_addrs : LocSet.t LocMap.t) :
    Jmp.t_full =
  match i.ins with
  | Fifth { target } ->
      {
        loc;
        jmp =
          (if
             (not (Byte8Map.mem (Loc.get_addr target) p.externs))
             && (String.starts_with ~prefix:"J" i.mnem
                || String.starts_with ~prefix:"M" i.mnem
                || String.starts_with ~prefix:"HLT" i.mnem)
           then JI (Jjump target)
           else if ILIR.Syn.Prog.get_ins p target |> Option.is_some then
             JC
               {
                 target = Cdirect { target; attr = () };
                 fallthrough = next;
                 attr = ();
               }
           else JT { target = Cdirect { target; attr = () }; attr = () });
        mnem = i.mnem;
      }
  | Sixth { target } ->
      {
        loc;
        jmp =
          (if String.equal i.mnem "RET" then JR { attr = target }
           else if String.equal i.mnem "CALL" then
             JC { target = Cind { target }; fallthrough = next; attr = () }
           else if LocMap.mem loc known_addrs then
             JI
               (Jjump_ind
                  {
                    target;
                    candidates = LocMap.find loc known_addrs;
                    sound = false;
                  })
           else JswitchStop target);
        mnem = i.mnem;
      }
  | Fourth { condition; target } ->
      {
        loc;
        jmp =
          JI (Jcbranch { condition; target_true = target; target_false = next });
        mnem = i.mnem;
      }
  | Seventh _ -> { loc; jmp = JI Junimplemented; mnem = i.mnem }
  | _ -> { loc; jmp = JI (Jfallthrough next); mnem = i.mnem }

let translate_block (p0 : ILIR.Syn.Prog.t) (fentry : Loc.t) (entry : Loc.t)
    (cf : ILIR.Shallow_CFA.t) (entries : LocSetD.t)
    (known_addrs : LocSet.t LocMap.t) : Block.t =
  let rec aux (loc : Loc.t) (acc : Inst.t_full list) : Block.t =
    let ninst = ILIR.Syn.Prog.get_ins_full p0 loc in
    match ninst with
    | None ->
        {
          fLoc = fentry;
          loc = entry;
          body = List.rev acc;
          jmp = { loc; jmp = JI Junimplemented; mnem = "" };
        }
    | Some i ->
        if ILIR.JumpG.G.mem_vertex cf.sound_jump loc then
          match ILIR.JumpG.G.succ cf.sound_jump loc with
          | [ jmp ] ->
              if LocSetD.mem jmp entries then
                {
                  fLoc = fentry;
                  loc = entry;
                  body = List.rev (translate_stmt loc i :: acc);
                  jmp = translate_jmp p0 loc i jmp [ jmp ] known_addrs;
                }
              else aux jmp (translate_stmt loc i :: acc)
          | jmps ->
              {
                fLoc = fentry;
                loc = entry;
                body = List.rev (translate_stmt loc i :: acc);
                jmp =
                  translate_jmp p0 loc i
                    (ILIR.Syn.Prog.fallthru p0 loc)
                    jmps known_addrs;
              }
        else
          {
            fLoc = fentry;
            loc = entry;
            body = List.rev (translate_stmt loc i :: acc);
            jmp = { loc; jmp = JI Junimplemented; mnem = i.mnem };
          }
  in

  aux entry []

let translate_func (p0 : ILIR.Syn.Prog.t) (nameo : String.t option)
    (entry : Byte8.t) (cf : ILIR.Shallow_CFA.t)
    (known_addrs : LocSet.t LocMap.t) : Func.t =
  let boundary_entries = fst cf.boundary_point in
  let other_block_entires =
    ILIR.JumpG.G.fold_vertex
      (fun l s ->
        match ILIR.JumpG.G.pred cf.sound_jump l with
        | [ p ] ->
            if ILIR.JumpG.G.out_degree cf.sound_jump p >= 2 then LocSet.add l s
            else s
        | _ -> LocSet.add l s)
      cf.sound_jump LocSet.empty
  in
  let entries = LocSetD.union other_block_entires boundary_entries in
  let blocks =
    LocSetD.to_seq entries
    |> Seq.map (fun e ->
           translate_block p0 (Loc.of_addr entry) e cf entries known_addrs)
    |> List.of_seq
  in
  {
    nameo;
    entry = Loc.of_addr entry;
    boundaries = boundary_entries;
    blocks;
    attr = ();
  }

let translate_prog (p0 : ILIR.Syn.Prog.t) (entries : Byte8.t list) : Prog.t =
  let funcs =
    List.map
      (fun e ->
        translate_func p0 None e
          (ILIR.Shallow_CFA.follow_flow p0 e)
          LocMap.empty)
      entries
  in
  { funcs; rom = p0.rom; rspec = p0.rspec; externs = p0.externs }

let translate_prog_from_cfa (p0 : ILIR.Syn.Prog.t)
    (cfa_res : (String.t * Byte8.t * ILIR.Shallow_CFA.t) list) : Prog.t =
  let funcs =
    List.map
      (fun (fname, e, cf) -> translate_func p0 (Some fname) e cf LocMap.empty)
      cfa_res
  in
  { funcs; rom = p0.rom; rspec = p0.rspec; externs = p0.externs }
