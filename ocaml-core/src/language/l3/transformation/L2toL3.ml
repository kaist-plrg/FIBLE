open Basic
open Basic_collection

let default_input =
  L2.REA.default.dependent_regs
  |> (function Top -> RegIdSet.empty | Set s -> s)
  |> RegIdSet.elements

let default_output =
  L2.REA.default.may_def_regs
  |> (function Top -> RegIdSet.empty | Set s -> s)
  |> RegIdSet.elements

let translate_jmp (j : L2.Jmp.t_full) (la : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : Jmp.t_full =
  let njmp : Jmp.t =
    match j.jmp with
    | Junimplemented -> Junimplemented
    | Jfallthrough n -> Jfallthrough n
    | Jjump n -> Jjump n
    | Jjump_ind { target; candidates; sound } ->
        Jjump_ind { target; candidates; sound }
    | Jcbranch { condition; target_true; target_false } ->
        Jcbranch { condition; target_true; target_false }
    | L2.Jmp.Jcall { reserved_stack; sp_diff; target; fallthrough } ->
        let inputs, outputs =
          match LocMap.find_opt target fMap with
          | Some (i, o) -> (i, o)
          | None -> (default_input, default_output)
        in
        Jcall
          {
            reserved_stack;
            sp_diff;
            outputs;
            inputs =
              inputs
              |> List.map (fun n ->
                     VarNode.Register { id = n; offset = 0l; width = 8l });
            target;
            fallthrough;
          }
    | Jcall_ind { reserved_stack; sp_diff; target; fallthrough } ->
        Jcall_ind { reserved_stack; sp_diff; target; fallthrough }
    | Jtailcall { reserved_stack; sp_diff; target } ->
        let inputs, outputs =
          match LocMap.find_opt target fMap with
          | Some (i, o) -> (i, o)
          | None -> (default_input, default_output)
        in
        let returns =
          snd la
          |> List.map (fun n ->
                 VarNode.Register { id = n; offset = 0l; width = 8l })
        in
        Jtailcall
          {
            reserved_stack;
            sp_diff;
            returns;
            outputs;
            inputs =
              inputs
              |> List.map (fun n ->
                     VarNode.Register { id = n; offset = 0l; width = 8l });
            target;
          }
    | L2.Jmp.Jtailcall_ind { reserved_stack; sp_diff; target } ->
        let returns =
          snd la
          |> List.map (fun n ->
                 VarNode.Register { id = n; offset = 0l; width = 8l })
        in
        Jtailcall_ind { reserved_stack; sp_diff; returns; target }
    | L2.Jmp.Jret ->
        let retvs =
          snd la
          |> List.map (fun n ->
                 VarNode.Register { id = n; offset = 0l; width = 8l })
        in
        Jret retvs
  in

  { jmp = njmp; loc = j.loc; mnem = j.mnem }

let translate_inst (i : L2.Inst.t_full) (la : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : Inst.t_full =
  i

let translate_block (b : L2.Block.t) (ga : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : Block.t =
  let body =
    List.fold_left (fun acci i -> acci @ [ translate_inst i ga fMap ]) [] b.body
  in
  { fLoc = b.fLoc; loc = b.loc; body; jmp = translate_jmp b.jmp ga fMap }

let translate_func (f : L2.Func.t) (a : RegId.t List.t * RegId.t List.t)
    (fMap : (RegId.t List.t * RegId.t List.t) LocMap.t) : Func.t =
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

let translate_prog_from_rea (p1 : L2.Prog.t)
    (rea_res : (L2.Func.t * L2.REA.astate) List.t) : Prog.t =
  let signature_list : (L2.Func.t * (RegId.t List.t * RegId.t List.t)) List.t =
    List.map
      (fun ((f, s) : L2.Func.t * L2.REA.astate) ->
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
      |> List.map (fun ((f, s) : L2.Func.t * 'a) -> (f.entry, s)))
  in
  let funcs = List.map (fun (f, a) -> translate_func f a fMap) signature_list in
  { sp_num = p1.sp_num; funcs; rom = p1.rom; externs = p1.externs }

let translate_prog (p1 : L2.Prog.t) : Prog.t =
  let rea_res = L2.REA.compute_all p1 in
  translate_prog_from_rea p1 rea_res
