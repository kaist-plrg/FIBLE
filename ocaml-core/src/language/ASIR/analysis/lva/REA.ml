open Common
open Basic_domain

type astate = {
  must_def_regs : RegIdSetD.t;
  may_def_regs : RegIdSetD.t;
  dependent_regs : RegIdSetD.t;
}

let pp fmt (a : astate) =
  Format.fprintf fmt "def_regs: %a, may_def_regs: %a, dependent_regs: %a"
    RegIdSetD.pp a.must_def_regs RegIdSetD.pp a.may_def_regs RegIdSetD.pp
    a.dependent_regs

let caller_saved_regs =
  [
    0l (* RAX *);
    8l (* RCX *);
    48l (* RSI *);
    56l (* RDI *);
    128l (* R8 *);
    136l (* R9 *);
    144l (* R10 *);
    152l (* R11 *);
  ]

let default =
  {
    must_def_regs =
      RegIdSetD.Set
        (List.map (fun x -> RegId.Register x) caller_saved_regs
        |> RegIdSet.of_list);
    may_def_regs =
      RegIdSetD.Set
        (List.map (fun x -> RegId.Register x) caller_saved_regs
        |> RegIdSet.of_list);
    dependent_regs = RegIdSetD.bot;
  }

let compute_dr_assignment (a : Assignable.t) : RegIdSet.t =
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

let compute_dd_inst (i : Inst.t) : astate =
  match i with
  | First (Load { pointer = Common.VarNode.Register rm; output; _ }) ->
      {
        must_def_regs = RegIdSetD.Set (RegIdSet.singleton output.id);
        may_def_regs = RegIdSetD.Set (RegIdSet.singleton output.id);
        dependent_regs = RegIdSetD.Set (RegIdSet.singleton rm.id);
      }
  | First
      (Store
        {
          pointer = Common.VarNode.Register rm;
          value = Common.VarNode.Register rs;
          _;
        }) ->
      {
        must_def_regs = RegIdSetD.bot;
        may_def_regs = RegIdSetD.bot;
        dependent_regs = RegIdSetD.Set (RegIdSet.of_list [ rm.id; rs.id ]);
      }
  | First
      (Store
        { pointer = Common.VarNode.Const _; value = Common.VarNode.Register rs })
  | Second (Sstore { value = Common.VarNode.Register rs; _ }) ->
      {
        must_def_regs = RegIdSetD.bot;
        may_def_regs = RegIdSetD.bot;
        dependent_regs = RegIdSetD.Set (RegIdSet.singleton rs.id);
      }
  | First (Load { pointer = Common.VarNode.Const _; output })
  | Second (Sload { output; _ }) ->
      {
        must_def_regs = RegIdSetD.Set (RegIdSet.singleton output.id);
        may_def_regs = RegIdSetD.Set (RegIdSet.singleton output.id);
        dependent_regs = RegIdSetD.bot;
      }
  | Third { expr; output } ->
      {
        must_def_regs = RegIdSetD.Set (RegIdSet.singleton output.id);
        may_def_regs = RegIdSetD.Set (RegIdSet.singleton output.id);
        dependent_regs = RegIdSetD.Set (compute_dr_assignment expr);
      }
  | Fourth _ ->
      {
        must_def_regs = RegIdSetD.bot;
        may_def_regs = RegIdSetD.bot;
        dependent_regs = RegIdSetD.bot;
      }
  | _ ->
      {
        must_def_regs = RegIdSetD.bot;
        may_def_regs = RegIdSetD.bot;
        dependent_regs = RegIdSetD.bot;
      }

let compute_dd_jmp (j : Jmp.t) : astate =
  match j with
  | JI (Jjump_ind { target = VarNode.Register r; _ })
  | JI (Jcbranch { condition = VarNode.Register r; _ })
  | JC { target = Cind { target = VarNode.Register r; _ }; _ } ->
      {
        must_def_regs = RegIdSetD.bot;
        may_def_regs = RegIdSetD.bot;
        dependent_regs = RegIdSetD.Set (RegIdSet.singleton r.id);
      }
  | _ ->
      {
        must_def_regs = RegIdSetD.bot;
        may_def_regs = RegIdSetD.bot;
        dependent_regs = RegIdSetD.bot;
      }

let accumulate_astate (a : astate) (b : astate) : astate =
  {
    must_def_regs = RegIdSetD.join a.must_def_regs b.must_def_regs;
    may_def_regs = RegIdSetD.join a.may_def_regs b.may_def_regs;
    dependent_regs =
      RegIdSetD.join a.dependent_regs
        (match (b.dependent_regs, a.must_def_regs) with
        | RegIdSetD.Top, _ -> RegIdSetD.Top
        | RegIdSetD.Set _, RegIdSetD.Top -> RegIdSetD.bot
        | RegIdSetD.Set s1, RegIdSetD.Set s2 ->
            RegIdSetD.Set (RegIdSet.diff s1 s2));
  }

let compute_dd_block (b : Block.t) : astate =
  List.fold_left
    (fun a (i : Inst.t_full) ->
      let b = compute_dd_inst i.ins in
      accumulate_astate a b)
    {
      must_def_regs = RegIdSetD.bot;
      may_def_regs = RegIdSetD.bot;
      dependent_regs = RegIdSetD.bot;
    }
    b.body
  |> Fun.flip accumulate_astate (compute_dd_jmp b.jmp.jmp)

module ICFG = Common.ICFGF.Make (Block)
module CG = Common.CGF.Make (Func)

module RegAnalysisDomain = struct
  type t = astate
  type data = astate LocMap.t * astate
  type edge = ICFG.G.E.t
  type vertex = ICFG.G.V.t
  type g = ICFG.G.t

  let direction : Graph.Fixpoint.direction = Forward

  let join_t (a : t) (b : t) : t =
    {
      must_def_regs = RegIdSetD.meet a.must_def_regs b.must_def_regs;
      may_def_regs = RegIdSetD.join a.may_def_regs b.may_def_regs;
      dependent_regs = RegIdSetD.join a.dependent_regs b.dependent_regs;
    }

  let join (a : data) (b : data) : data = (fst a, join_t (snd a) (snd b))

  let le (a : t) (b : t) : bool =
    RegIdSetD.le b.must_def_regs a.must_def_regs
    && RegIdSetD.le a.may_def_regs b.may_def_regs
    && RegIdSetD.le a.dependent_regs b.dependent_regs

  let le_data (a : data) (b : data) : bool =
    RegIdSetD.le (snd b).must_def_regs (snd a).must_def_regs
    && RegIdSetD.le (snd a).may_def_regs (snd b).may_def_regs
    && RegIdSetD.le (snd a).dependent_regs (snd b).dependent_regs

  let equal (a : data) (b : data) : bool = le_data a b && le_data b a

  let analyze ((bs, e, bf) : ICFG.G.E.t) (a : data) : data =
    match e with
    | ICFG.EdgeLabel.Inner ->
        (fst a, accumulate_astate (snd a) (compute_dd_block bs.block))
    | ICFG.EdgeLabel.Flow -> (
        match bs.block.jmp.jmp with
        | JC { target = Cdirect { target; _ }; _ } ->
            ( fst a,
              accumulate_astate (snd a)
                (LocMap.find_opt target (fst a) |> Option.value ~default) )
        | JC { target = Cind _; _ } -> (fst a, accumulate_astate (snd a) default)
        | _ -> a)

  let bot =
    {
      must_def_regs = RegIdSetD.top;
      may_def_regs = RegIdSetD.bot;
      dependent_regs = RegIdSetD.bot;
    }

  let init =
    {
      must_def_regs = RegIdSetD.bot;
      may_def_regs = RegIdSetD.bot;
      dependent_regs = RegIdSetD.bot;
    }
end

module RegAnalysis = Graph.Fixpoint.Make (ICFG.G) (RegAnalysisDomain)

let compute_dd_func (f : Func.t) (m : astate LocMap.t) : astate =
  let analyze_res =
    RegAnalysis.analyze
      (fun v ->
        if v.block.loc = f.entry && v.time = Pre then (m, RegAnalysisDomain.init)
        else (m, RegAnalysisDomain.bot))
      (ICFG.to_graph f.blocks)
  in
  Func.get_blocks f
  |> List.fold_left
       (fun a b ->
         RegAnalysisDomain.join_t a
           (snd (analyze_res { block = b; time = Post })))
       RegAnalysisDomain.bot

let rec compute_funcs_fixpoint (m : astate LocMap.t)
    (funcs : (astate * Func.t) list) : astate LocMap.t =
  if
    List.for_all
      (fun ((s, f) : astate * Func.t) ->
        match LocMap.find_opt f.entry m with
        | Some m -> RegAnalysisDomain.le s m
        | None -> false)
      funcs
  then m
  else
    let nm =
      List.fold_left
        (fun m ((s, f) : astate * Func.t) ->
          LocMap.update f.entry
            (function
              | None -> Some s | Some s1 -> Some (RegAnalysisDomain.join_t s s1))
            m)
        m funcs
    in
    let nfuncs =
      List.map
        (fun ((s, f) : astate * Func.t) -> (compute_dd_func f nm, f))
        funcs
    in
    compute_funcs_fixpoint nm nfuncs

let compute_all (p : Prog.t) : (Func.t * astate) List.t =
  let scc_list = CG.CG_Components.scc_list (CG.to_cg p.funcs) in
  let analysis_res =
    List.fold_left
      (fun m scc ->
        compute_funcs_fixpoint m
          (scc |> List.map (fun (f : Func.t) -> (RegAnalysisDomain.bot, f))))
      LocMap.empty scc_list
  in
  List.map
    (fun (f : Func.t) ->
      ( f,
        LocMap.find_opt f.entry analysis_res
        |> Option.value ~default:RegAnalysisDomain.bot ))
    p.funcs

(* 1. REA 하고 vararg인지 분석을 한번 함
   2. vararg의 경우에는 해당 call-site에서 모든 path에서 set된 register 확인해서 callvararg instruction 추가
*)
