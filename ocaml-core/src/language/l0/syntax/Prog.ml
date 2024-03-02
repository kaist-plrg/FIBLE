open StdlibExt
open Basic
open Basic_collection
open Common_language

type t = {
  ins_mem : Addr.t -> (int * Inst.t_full list) option;
  rom : DMem.t;
  rspec : Int32.t Int32Map.t;
  externs : String.t AddrMap.t;
}

let get_ins (p : t) (loc : Loc.t) : Inst.t option =
  match p.ins_mem (fst loc) with
  | None -> None
  | Some (_, ins_list) ->
      if snd loc < List.length ins_list then
        Some (List.nth ins_list (snd loc)).ins
      else None

let get_ins_full (p : t) (loc : Loc.t) : Inst.t_full option =
  match p.ins_mem (fst loc) with
  | None -> None
  | Some (_, ins_list) ->
      if snd loc < List.length ins_list then Some (List.nth ins_list (snd loc))
      else None

let get_rom_byte (p : t) (addr : Addr.t) : Char.t = DMem.get_byte p.rom addr

let get_rom (p : t) (addr : Addr.t) (width : Int32.t) :
    Common_language.NumericValue.t =
  let rec aux (addr : Addr.t) (width : Int32.t) (acc : Char.t list) :
      Char.t list =
    if width = 0l then acc
    else
      let c = get_rom_byte p addr in
      aux (Addr.succ addr) (Int32.pred width) (c :: acc)
  in
  let chars = aux addr width [] |> List.rev in
  Common_language.NumericValue.of_chars chars

let reposition_sn (vs : int * Inst.t_full list) (s : Loc.t) : Loc.t =
  match vs with
  | v, is ->
      if snd s < List.length is then s
      else (Int64.add (fst s) (Int64.of_int v), 0)

let reposition_sn_set (vs : int * Inst.t_full list) (s : LocSet.t) : LocSet.t =
  LocSet.map (reposition_sn vs) s

let fallthru (p : t) (l : Loc.t) : Loc.t =
  let nl = (fst l, succ (snd l)) in
  match p.ins_mem (fst l) with Some vs -> reposition_sn vs nl | None -> nl
