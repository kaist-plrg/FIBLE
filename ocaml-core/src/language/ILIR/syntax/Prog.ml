open StdlibExt
open Common

type t = {
  ins_mem : Byte8.t -> (int * Inst.t_full list) option;
  rom : DMem.t;
  rspec : Int32.t Int32Map.t;
  externs : String.t Byte8Map.t;
}

let get_ins (p : t) (loc : Loc.t) : Inst.t option =
  match p.ins_mem (Loc.get_addr loc) with
  | None -> None
  | Some (_, ins_list) ->
      if Loc.get_seq loc < List.length ins_list then
        Some (List.nth ins_list (Loc.get_seq loc)).ins
      else None

let get_ins_full (p : t) (loc : Loc.t) : Inst.t_full option =
  match p.ins_mem (Loc.get_addr loc) with
  | None -> None
  | Some (_, ins_list) ->
      if Loc.get_seq loc < List.length ins_list then
        Some (List.nth ins_list (Loc.get_seq loc))
      else None

let get_rom_byte (p : t) (addr : Byte8.t) : Char.t = DMem.get_byte p.rom addr

let get_rom (p : t) (addr : Byte8.t) (width : Int32.t) : Common.NumericValue.t =
  let rec aux (addr : Byte8.t) (width : Int32.t) (acc : Char.t list) :
      Char.t list =
    if width = 0l then acc
    else
      let c = get_rom_byte p addr in
      aux (Byte8.succ addr) (Int32.pred width) (c :: acc)
  in
  let chars = aux addr width [] |> List.rev in
  Common.NumericValue.of_chars chars

let reposition_sn (vs : int * Inst.t_full list) (s : Loc.t) : Loc.t =
  match vs with
  | v, is ->
      if Loc.get_seq s < List.length is then s
      else Loc.of_addr (Int64.add (Loc.get_addr s) (Int64.of_int v))

let reposition_sn_set (vs : int * Inst.t_full list) (s : LocSet.t) : LocSet.t =
  LocSet.map (reposition_sn vs) s

let fallthru (p : t) (l : Loc.t) : Loc.t =
  let nl = Loc.of_addr_seq (Loc.get_addr l, succ (Loc.get_seq l)) in
  match p.ins_mem (Loc.get_addr l) with
  | Some vs -> reposition_sn vs nl
  | None -> nl
