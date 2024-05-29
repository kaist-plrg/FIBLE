open Common
open Basic_domain

module ALoc = struct
  type t = ALoc of Loc.t | AStart

  let pp fmt (a : t) =
    match a with
    | ALoc l -> Format.fprintf fmt "%a" Loc.pp l
    | AStart -> Format.fprintf fmt "start"

  let compare (a : t) (b : t) : int = compare a b
end

module ALocSet = struct
  include Set.Make (ALoc)

  let pp fmt (s : t) =
    Format.fprintf fmt "%a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space ALoc.pp)
      (elements s)
end

type abstr = {
  reg_def_locs : ALocSet.t * ALocSet.t RegIdMap.t;
  store_locs : ALocSet.t;
}

let pp fmt (a : abstr) =
  let pp_reg_def_locs fmt ((r, locs) : RegId.t * ALocSet.t) =
    Format.fprintf fmt "%a -> {%a}" RegId.pp r ALocSet.pp locs
  in
  let pp_store_locs fmt (locs : ALocSet.t) =
    Format.fprintf fmt "{%a}" ALocSet.pp locs
  in
  Format.fprintf fmt "{reg_def_locs = %a; store_locs = %a}"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_reg_def_locs)
    (RegIdMap.bindings (snd a.reg_def_locs))
    pp_store_locs a.store_locs

let top : abstr =
  { reg_def_locs = (ALocSet.empty, RegIdMap.empty); store_locs = ALocSet.empty }

let get_reg_def_locs (a : abstr) (r : RegId.t) : ALocSet.t =
  RegIdMap.find_opt r (snd a.reg_def_locs)
  |> Option.value ~default:(fst a.reg_def_locs)

let update_reg_def_loc (a : abstr) (r : RegId.t) (l : Loc.t) : abstr =
  {
    a with
    reg_def_locs =
      ( fst a.reg_def_locs,
        RegIdMap.add r (ALocSet.singleton (ALoc l)) (snd a.reg_def_locs) );
  }

let update_store_loc (a : abstr) (l : Loc.t) : abstr =
  { a with store_locs = ALocSet.add (ALoc l) a.store_locs }

let has_mem (a : Assignable.t) : bool =
  match a with
  | Assignable.Avar (VarNode.Ram _)
  | Assignable.Auop (_, VarNode.Ram _)
  | Assignable.Abop (_, _, VarNode.Ram _)
  | Assignable.Abop (_, VarNode.Ram _, _) ->
      true
  | _ -> false

let compute_ud_assignment (a : Assignable.t) : RegIdSet.t =
  match a with
  | Assignable.Avar (VarNode.Register r)
  | Assignable.Auop (_, VarNode.Register r)
  | Assignable.Abop (_, VarNode.Register r, VarNode.Const _)
  | Assignable.Abop (_, VarNode.Register r, VarNode.Ram _)
  | Assignable.Abop (_, VarNode.Const _, VarNode.Register r)
  | Assignable.Abop (_, VarNode.Ram _, VarNode.Register r) ->
      RegIdSet.singleton r.id
  | Assignable.Avar (VarNode.Const _)
  | Assignable.Avar (VarNode.Ram _)
  | Assignable.Auop (_, VarNode.Const _)
  | Assignable.Auop (_, VarNode.Ram _)
  | Assignable.Abop (_, VarNode.Const _, VarNode.Const _)
  | Assignable.Abop (_, VarNode.Const _, VarNode.Ram _)
  | Assignable.Abop (_, VarNode.Ram _, VarNode.Const _)
  | Assignable.Abop (_, VarNode.Ram _, VarNode.Ram _) ->
      RegIdSet.empty
  | Assignable.Abop (_, VarNode.Register r1, VarNode.Register r2) ->
      RegIdSet.of_list [ r1.id; r2.id ]

let compute_ud_inst (astate : abstr) (i : Inst.t_full) : abstr * ALocSet.t =
  match i.ins with
  | First (Load { pointer = Common.VarNode.Register rm; output; _ }) ->
      ( update_reg_def_loc astate output.id i.loc,
        ALocSet.union (get_reg_def_locs astate rm.id) astate.store_locs )
  | First (Load { pointer = Common.VarNode.Const _; output; _ }) ->
      (update_reg_def_loc astate output.id i.loc, astate.store_locs)
  | First
      (Store
        {
          pointer = Common.VarNode.Register rm;
          value = Common.VarNode.Register rs;
          _;
        }) ->
      ( update_store_loc astate i.loc,
        ALocSet.union
          (get_reg_def_locs astate rm.id)
          (get_reg_def_locs astate rs.id) )
  | First
      (Store
        {
          pointer = Common.VarNode.Const _;
          value = Common.VarNode.Register rs;
          _;
        }) ->
      (update_store_loc astate i.loc, get_reg_def_locs astate rs.id)
  | Second { expr; output } ->
      ( update_reg_def_loc astate output.id i.loc,
        (compute_ud_assignment expr |> Fun.flip RegIdSet.fold)
          (fun r s -> ALocSet.union (get_reg_def_locs astate r) s)
          (if has_mem expr then astate.store_locs else ALocSet.empty) )
  | Third _ -> (astate, ALocSet.empty)
  | _ -> (astate, ALocSet.empty)

let compute_ud_jmp (astate : abstr) (j : Jmp.t_full) : abstr * ALocSet.t =
  match j.jmp with
  | JI (Jjump_ind { target = VarNode.Register r; _ })
  | JI (Jcbranch { condition = VarNode.Register r; _ })
  | JC { target = Cind { target = VarNode.Register r }; _ }
  | JswitchStop (VarNode.Register r) ->
      (astate, get_reg_def_locs astate r.id)
  | JI (Jjump_ind { target = VarNode.Ram _; _ })
  | JI (Jcbranch { condition = VarNode.Ram _; _ })
  | JC { target = Cind { target = VarNode.Ram _; _ }; _ }
  | Jmp.JswitchStop (VarNode.Ram _) ->
      (astate, astate.store_locs)
  | _ -> (astate, ALocSet.empty)

let compute_ud_block (astate : abstr) (b : Block.t) : abstr =
  List.fold_left
    (fun a (i : Inst.t_full) -> fst (compute_ud_inst a i))
    astate b.body
  |> Fun.flip compute_ud_jmp b.jmp
  |> fst

module RegAnalysisDomain = struct
  type t = abstr
  type data = abstr
  type edge = ICFG.G.E.t
  type vertex = ICFG.G.V.t
  type g = ICFG.G.t

  let direction : Graph.Fixpoint.direction = Forward

  let join (a : t) (b : t) : t =
    {
      reg_def_locs =
        ( ALocSet.union (fst a.reg_def_locs) (fst b.reg_def_locs),
          RegIdMap.merge
            (fun _ ax bx ->
              match (ax, bx) with
              | None, None -> None
              | Some ax, None -> Some (ALocSet.union ax (fst b.reg_def_locs))
              | None, Some bx -> Some (ALocSet.union (fst a.reg_def_locs) bx)
              | Some ax, Some bx -> Some (ALocSet.union ax bx))
            (snd a.reg_def_locs) (snd b.reg_def_locs) );
      store_locs = ALocSet.union a.store_locs b.store_locs;
    }

  let equal (a : t) (b : t) : bool =
    ALocSet.equal (fst a.reg_def_locs) (fst b.reg_def_locs)
    && RegIdMap.equal ALocSet.equal (snd a.reg_def_locs) (snd b.reg_def_locs)
    && ALocSet.equal a.store_locs b.store_locs

  let analyze ((bs, e, bf) : ICFG.G.E.t) (a : t) : t =
    match e with
    | ICFG.EdgeLabel.Inner -> compute_ud_block a bs.block
    | ICFG.EdgeLabel.Flow -> a

  let init =
    {
      reg_def_locs = (ALocSet.singleton ALoc.AStart, RegIdMap.empty);
      store_locs = ALocSet.empty;
    }

  let bot =
    {
      reg_def_locs = (ALocSet.empty, RegIdMap.empty);
      store_locs = ALocSet.empty;
    }
end

module RegAnalysis = Graph.Fixpoint.Make (ICFG.G) (RegAnalysisDomain)

let compute_ud_func_with_graph (f : Func.t) (g : ICFG.G.t) : ALocSet.t LocMap.t
    =
  let analyze_res =
    RegAnalysis.analyze
      (fun v ->
        if v.block.loc = f.entry && v.time = Pre then RegAnalysisDomain.init
        else RegAnalysisDomain.bot)
      g
  in
  List.fold_left
    (fun m (b : Block.t) ->
      let a = analyze_res { block = b; time = Pre } in
      List.fold_left
        (fun ((a, m) : abstr * ALocSet.t LocMap.t) (i : Inst.t_full) ->
          let a, ns = compute_ud_inst a i in
          (a, LocMap.add i.loc ns m))
        (a, m) b.body
      |> fun (a, m) ->
      let _, ns = compute_ud_jmp a b.jmp in
      LocMap.add b.jmp.loc ns m)
    LocMap.empty f.blocks

let compute_ud_func (f : Func.t) : ALocSet.t LocMap.t =
  compute_ud_func_with_graph f (ICFG.to_graph f.blocks)
