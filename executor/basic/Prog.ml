open StdlibExt;;

type t = {
    ins_mem: Addr.t -> (int * (Inst.t list)) option;
    rom: Addr.t -> int64;
    entry_addr: Addr.t
}

let get_ins (p: t) (loc: Loc.t): Inst.t option =
  match p.ins_mem (fst loc) with
  | None -> None
  | Some (_, ins_list) ->
      if (snd loc) < (List.length ins_list) then
          Some (List.nth ins_list (snd loc))
      else
          None

let get_rom (p: t) (addr: Addr.t) (width: int32): int64 =
    let v_full = p.rom addr in
    Int64Ext.cut_width v_full width