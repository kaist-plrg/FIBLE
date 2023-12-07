open Basic
open Basic_collection

let default_input =
  L3.REA.default.dependent_regs
  |> (function Top -> RegIdSet.empty | Set s -> s)
  |> RegIdSet.elements

let default_output =
  L3.REA.default.may_def_regs
  |> (function Top -> RegIdSet.empty | Set s -> s)
  |> RegIdSet.elements

let translate_jmp (j : L3.Jmp.t_full) (la : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : L4.Jmp.t_full =
  let njmp : L4.Jmp.t =
    match j.jmp with
    | Junimplemented -> Junimplemented
    | Jfallthrough n -> Jfallthrough n
    | Jjump n -> Jjump n
    | Jjump_ind vs -> Jjump_ind vs
    | Jcbranch vs -> Jcbranch vs
    | L3.Jmp.Jcall (spdiff, target, retn) ->
        let inputs, outputs =
          match LocMap.find_opt target fMap with
          | Some (i, o) -> (i, o)
          | None -> (default_input, default_output)
        in
        Jcall
          ( spdiff,
            outputs,
            inputs
            |> List.map (fun n ->
                   VarNode.Register { id = n; offset = 0l; width = 8l }),
            target,
            retn )
    | L3.Jmp.Jcall_ind (spdiff, targetvn, retn) ->
        Jcall_ind (spdiff, targetvn, retn)
    | L3.Jmp.Jtailcall (spdiff, target) ->
        let inputs, outputs =
          match LocMap.find_opt target fMap with
          | Some (i, o) -> (i, o)
          | None -> (default_input, default_output)
        in
        let retvs =
          snd la
          |> List.map (fun n ->
                 VarNode.Register { id = n; offset = 0l; width = 8l })
        in
        Jtailcall
          ( spdiff,
            retvs,
            outputs,
            inputs
            |> List.map (fun n ->
                   VarNode.Register { id = n; offset = 0l; width = 8l }),
            target )
    | L3.Jmp.Jtailcall_ind (spdiff, targetvn) ->
        let retvs =
          snd la
          |> List.map (fun n ->
                 VarNode.Register { id = n; offset = 0l; width = 8l })
        in
        Jtailcall_ind (spdiff, retvs, targetvn)
    | L3.Jmp.Jret ->
        let retvs =
          snd la
          |> List.map (fun n ->
                 VarNode.Register { id = n; offset = 0l; width = 8l })
        in
        Jret retvs
  in

  { jmp = njmp; loc = j.loc; mnem = j.mnem }

let translate_inst (i : L3.Inst.t_full) (la : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : L4.Inst.t_full =
  i

let translate_block (b : L3.Block.t) (ga : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : L4.Block.t =
  let body =
    List.fold_left (fun acci i -> acci @ [ translate_inst i ga fMap ]) [] b.body
  in
  { fLoc = b.fLoc; loc = b.loc; body; jmp = translate_jmp b.jmp ga fMap }

let translate_func (f : L3.Func.t) (a : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : L4.Func.t =
  {
    nameo = f.nameo;
    entry = f.entry;
    blocks = List.map (fun b -> translate_block b a fMap) f.blocks;
    boundaries = f.boundaries;
    sp_diff = f.sp_diff;
    sp_boundary = f.sp_boundary;
    inputs = fst a;
    outputs = snd a;
  }

let translate_prog_from_rea (p1 : L3.Prog.t)
    (rea_res : (L3.Func.t * L3.REA.astate) List.t) : L4.Prog.t =
  let signature_list : (L3.Func.t * (RegId.t List.t * RegId.t List.t)) List.t =
    List.map
      (fun ((f, s) : L3.Func.t * L3.REA.astate) ->
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
      |> List.map (fun ((f, s) : L3.Func.t * 'a) -> (f.entry, s)))
  in
  let funcs = List.map (fun (f, a) -> translate_func f a fMap) signature_list in
  { sp_num = p1.sp_num; funcs; rom = p1.rom; externs = p1.externs }

let translate_prog (p1 : L3.Prog.t) : L4.Prog.t =
  let rea_res = L3.REA.compute_all p1 in
  translate_prog_from_rea p1 rea_res
