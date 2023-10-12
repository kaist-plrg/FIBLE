open StdlibExt
open Basic
open Basic_domain

type t = {
  ins_mem : Addr.t -> (int * Inst.t list) option;
  rom : Addr.t -> int64;
}

let get_ins (p : t) (loc : Loc.t) : Inst.t option =
  match p.ins_mem (fst loc) with
  | None -> None
  | Some (_, ins_list) ->
      if snd loc < List.length ins_list then Some (List.nth ins_list (snd loc))
      else None

let get_rom (p : t) (addr : Addr.t) (width : int32) : int64 =
  let v_full = p.rom addr in
  Int64Ext.cut_width v_full width

let reposition_sn (vs : int * Inst.t list) (s : Loc.t) : Loc.t =
  match vs with
  | v, is ->
      if snd s < List.length is then s
      else (Int64.add (fst s) (Int64.of_int v), 0)

let reposition_sn_set (vs : int * Inst.t list) (s : LocSetD.t) : LocSetD.t =
  LocSetD.map (reposition_sn vs) s

let fallthru (p : t) (l : Loc.t) : Loc.t =
  let nl = (fst l, succ (snd l)) in
  match p.ins_mem (fst l) with Some vs -> reposition_sn vs nl | None -> nl
