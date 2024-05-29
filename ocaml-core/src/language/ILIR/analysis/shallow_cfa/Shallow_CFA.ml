open StdlibExt
open Common
open Basic_domain
open Value_domain

type heuristic_result = HrStop | HrSound of LocSetD.t | HrExit | HrFallthrough

type flow_heurstic_type =
  Prog.t ->
  Loc.t ->
  Inst.t ->
  Mnemonic.t ->
  LocSet.t LocMap.t ->
  heuristic_result

let gen_bb_single (h : flow_heurstic_type) (p : Prog.t) (l : Loc.t) (i : Inst.t)
    (m : Mnemonic.t) (known_addrs : LocSet.t LocMap.t) :
    Loc.t option * Loc.t option =
  match h p l i m known_addrs with
  | HrStop -> (None, None)
  | HrSound s -> (None, None)
  | HrExit -> (None, Some l)
  | HrFallthrough -> (Some (Prog.fallthru p l), Some l)

let gen_jump_single (h : flow_heurstic_type) (p : Prog.t) (l : Loc.t)
    (i : Inst.t) (m : Mnemonic.t) (known_addrs : LocSet.t LocMap.t) : LocSetD.t
    =
  match h p l i m known_addrs with
  | HrStop -> LocSetD.empty
  | HrSound s -> s
  | HrExit -> LocSetD.empty
  | HrFallthrough -> LocSetD.empty

let nonjumpv (fallthru : Loc.t) (_ : 'a) = HrSound (LocSetD.singleton fallthru)

let flow_heuristic_simple (p : Prog.t) (l : Loc.t) (i : Inst.t) (m : Mnemonic.t)
    (known_addrs : LocSet.t LocMap.t) : heuristic_result =
  let ft = Prog.fallthru p l in
  Inst.fold (nonjumpv ft) (nonjumpv ft) (nonjumpv ft)
    (fun { target; _ } -> HrSound (LocSetD.of_list [ target; ft ]))
    (fun { target } ->
      match m with
      | "CALL" -> HrFallthrough
      | m ->
          if Byte8Map.mem (Loc.get_addr target) p.externs then HrFallthrough
          else if
            String.starts_with ~prefix:"J" m || String.starts_with ~prefix:"M" m
          then HrSound (LocSetD.singleton target)
          else HrExit)
    (fun { target } ->
      match m with
      | "RET" -> HrExit
      | "CALL" -> HrFallthrough
      | _ ->
          LocMap.find_opt l known_addrs
          |> Option.map (fun s -> HrSound s)
          |> Option.value ~default:HrStop)
    (fun _ -> HrExit)
    i

type t = {
  boundary_point : BoundaryPointD.t;
  sound_jump : JumpG.G.t;
  visited : LocSetD.t;
}

let init (p : Prog.t) (l : Loc.t) : t =
  {
    boundary_point = (LocSetD.singleton l, LocSetD.empty);
    sound_jump = JumpG.G.add_vertex JumpG.G.empty l;
    visited = LocSetD.empty;
  }

let post_worklist (p : Prog.t) (ca : t) (l : Loc.t)
    (known_addrs : LocSet.t LocMap.t) : t * Loc.t List.t =
  if LocSetD.mem l ca.visited then (ca, [])
  else
    match Prog.get_ins_full p l with
    | Some { ins = i; mnem = m } ->
        let eno, exo =
          gen_bb_single flow_heuristic_simple p l i m known_addrs
        in
        let nb =
          Option.map (fun l -> BoundaryPointD.add_entry ca.boundary_point l) eno
          |> Option.value ~default:ca.boundary_point
        in
        let nb =
          Option.map (fun l -> BoundaryPointD.add_exit nb l) exo
          |> Option.value ~default:nb
        in
        let nextJumpSet =
          gen_jump_single flow_heuristic_simple p l i m known_addrs
        in
        let nj =
          LocSetD.fold
            (fun ln g -> JumpG.G.add_edge (JumpG.G.add_vertex g ln) l ln)
            nextJumpSet ca.sound_jump
        in
        let nj =
          Option.map (fun l -> JumpG.G.add_vertex nj l) eno
          |> Option.value ~default:nj
        in
        let nvisited = LocSetD.add l ca.visited in
        let ulist =
          Option.map (fun s -> LocSetD.add s nextJumpSet) eno
          |> Option.value ~default:nextJumpSet
          |> LocSetD.to_seq |> List.of_seq
        in
        ({ boundary_point = nb; sound_jump = nj; visited = nvisited }, ulist)
    | _ -> (ca, [])

let rec a_fixpoint_worklist (p : Prog.t) (c : t) (ls : Loc.t List.t)
    (known_addrs : LocSet.t LocMap.t) : t =
  [%log
    debug "ls: %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Loc.pp)
      ls];
  match ls with
  | [] -> c
  | l :: ls ->
      let nc, newLs = post_worklist p c l known_addrs in
      a_fixpoint_worklist p nc
        (ls
        @ List.filter
            (fun l ->
              (not (List.mem l ls)) && Prog.get_ins p l |> Option.is_some)
            newLs)
        known_addrs

let follow_flow (p : Prog.t) (e : Byte8.t) : t =
  [%log debug "entry: %a" Byte8.pp e];
  a_fixpoint_worklist p
    (init p (Loc.of_addr e))
    (Loc.of_addr e :: []) LocMap.empty

let follow_flow_with_known_addr (p : Prog.t) (e : Byte8.t)
    (known_addrs : LocSet.t LocMap.t) : t =
  [%log debug "entry: %a" Byte8.pp e];
  a_fixpoint_worklist p
    (init p (Loc.of_addr e))
    (Loc.of_addr e :: []) known_addrs
