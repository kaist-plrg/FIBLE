open Common
open Syn

let default_input =
  ASIR.REA.default.dependent_regs
  |> (function Top -> RegIdSet.empty | Set s -> s)
  |> RegIdSet.elements

let default_output =
  ASIR.REA.default.may_def_regs
  |> (function Top -> RegIdSet.empty | Set s -> s)
  |> RegIdSet.elements

let translate_jmp (j : ASIR.Syn.Jmp.t_full)
    (la : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : Jmp.t_full =
  let njmp : Jmp.t =
    match j.jmp with
    | JI j -> JI j
    | JC
        {
          target = Cdirect { target; _ };
          fallthrough;
          attr = { reserved_stack; sp_diff };
        } ->
        let inputs, outputs =
          match LocMap.find_opt target fMap with
          | Some (i, o) -> (i, o)
          | None -> (default_input, default_output)
        in
        JC
          {
            target =
              Cdirect
                {
                  target;
                  attr =
                    {
                      outputs;
                      inputs =
                        inputs
                        |> List.map (fun n ->
                               VarNodeF.Register
                                 { id = n; offset = 0l; width = 8l });
                    };
                };
            fallthrough;
            attr = { reserved_stack; sp_diff };
          }
    | JC
        {
          target = Cind { target; _ };
          fallthrough;
          attr = { reserved_stack; sp_diff };
        } ->
        JC
          {
            target = Cind { target };
            fallthrough;
            attr = { reserved_stack; sp_diff };
          }
    | JT { target = Cdirect { target; _ }; attr = { reserved_stack; sp_diff } }
      ->
        let inputs, outputs =
          match LocMap.find_opt target fMap with
          | Some (i, o) -> (i, o)
          | None -> (default_input, default_output)
        in
        let returns =
          snd la
          |> List.map (fun n ->
                 VarNodeF.Register { id = n; offset = 0l; width = 8l })
        in
        JT
          {
            target =
              Cdirect
                {
                  target;
                  attr =
                    {
                      outputs;
                      inputs =
                        inputs
                        |> List.map (fun n ->
                               VarNodeF.Register
                                 { id = n; offset = 0l; width = 8l });
                    };
                };
            attr = { reserved_stack; sp_diff; returns };
          }
    | JT { target = Cind { target; _ }; attr = { reserved_stack; sp_diff } } ->
        let returns =
          snd la
          |> List.map (fun n ->
                 VarNodeF.Register { id = n; offset = 0l; width = 8l })
        in
        JT
          {
            target = Cind { target };
            attr = { reserved_stack; sp_diff; returns };
          }
    | JR { attr = () } ->
        let retvs =
          snd la
          |> List.map (fun n ->
                 VarNodeF.Register { id = n; offset = 0l; width = 8l })
        in
        JR { attr = retvs }
  in

  { jmp = njmp; loc = j.loc; mnem = j.mnem }

let translate_inst (i : ASIR.Syn.Inst.t_full)
    (la : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : Inst.t_full =
  i

let translate_block (b : ASIR.Syn.Block.t)
    (ga : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : Block.t =
  let body =
    List.fold_left (fun acci i -> acci @ [ translate_inst i ga fMap ]) [] b.body
  in
  { fLoc = b.fLoc; loc = b.loc; body; jmp = translate_jmp b.jmp ga fMap }

let translate_func (f : ASIR.Syn.Func.t) (a : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : Func.t =
  {
    nameo = f.nameo;
    entry = f.entry;
    blocks = List.map (fun b -> translate_block b a fMap) f.blocks;
    boundaries = f.boundaries;
    attr =
      {
        sp_diff = f.attr.sp_diff;
        sp_boundary = f.attr.sp_boundary;
        inputs = fst a;
        outputs = snd a;
      };
  }

let translate_prog_from_rea (p1 : ASIR.Syn.Prog.t)
    (rea_res : (ASIR.Syn.Func.t * ASIR.REA.astate) List.t) : Prog.t =
  let signature_list :
      (ASIR.Syn.Func.t * (RegId.t List.t * RegId.t List.t)) List.t =
    List.map
      (fun ((f, s) : ASIR.Syn.Func.t * ASIR.REA.astate) ->
        ( f,
          ( s.dependent_regs
            |> (function Top -> RegIdSet.empty | Set s -> s)
            |> RegIdSet.elements,
            s.may_def_regs
            |> (function Top -> RegIdSet.empty | Set s -> s)
            |> RegIdSet.elements ) ))
      rea_res
  in
  let fMap =
    LocMap.of_list
      (signature_list
      |> List.map (fun ((f, s) : ASIR.Syn.Func.t * 'a) -> (f.entry, s)))
  in
  let funcs = List.map (fun (f, a) -> translate_func f a fMap) signature_list in
  {
    sp_num = p1.sp_num;
    fp_num = p1.fp_num;
    funcs;
    rom = p1.rom;
    rspec = p1.rspec;
    externs = p1.externs;
    objects = p1.objects;
  }

let translate_prog (p1 : ASIR.Syn.Prog.t) : Prog.t =
  let rea_res = ASIR.REA.compute_all p1 in
  translate_prog_from_rea p1 rea_res
