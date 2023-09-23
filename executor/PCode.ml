
type varNodeI =
 | Unique of int64
 | Register of int64
 | Const of int64
 | Ram of int64


type varNode = {
    varNode_node: varNodeI;
    varNode_width: int32
}

type bop =
 | Bpiece | Bsubpiece | Bint_equal | Bint_notequal
 | Bint_less | Bint_sless | Bint_lessequal | Bint_slessequal
 | Bint_add | Bint_sub | Bint_carry | Bint_scarry
 | Bint_sborrow | Bint_xor | Bint_and | Bint_or
 | Bint_left | Bint_right | Bint_sright | Bint_mult
 | Bint_div | Bint_rem | Bint_sdiv | Bint_srem
 | Bbool_xor | Bbool_and | Bbool_or | Bfloat_equal
 | Bfloat_notequal | Bfloat_less | Bfloat_lessequal
 | Bfloat_add | Bfloat_sub | Bfloat_mult | Bfloat_div

type uop =
 | Upopcount | Ulzcount | Uint_zext | Uint_sext
 | Uint_2comp | Uint_negate | Ubool_negate | Ufloat_neg
 | Ufloat_abs | Ufloat_sqrt | Ufloat_ceil
 | Ufloat_floor | Ufloat_round | Ufloat_nan
 | Uint2float | Ufloat2float | Utrunc


type assignable =
 | Avar of varNode
 | Auop of (uop * varNode)
 | Abop of (bop * varNode * varNode)

type jannotation =
 | Jbranch
 | Jcall


 type jiannotation =
 | JIbranch
 | JIcall
 | JIret

type inst =
 | Iunimplemented
 | Iload of (varNode * varNode * varNode)
 | Istore of (varNode * varNode * varNode)
 | Ijump of (jannotation * varNode)
 | Ijump_ind of (jiannotation * varNode)
 | Icbranch of (varNode * varNode)
 | Iassignment of (assignable * varNode)
 | INop

type addr = int64

type loc = addr * int

type prog = {
    ins_mem: addr -> (int * (inst list)) option;
    rom: addr -> int64;
    entry_addr: addr
}

let get_ins (p: prog) (loc: loc): inst option =
    match p.ins_mem (fst loc) with
    | None -> None
    | Some (_, ins_list) ->
        if (snd loc) < (List.length ins_list) then
            Some (List.nth ins_list (snd loc))
        else
            None


let cut_width (v: int64) (width: int32): int64 =
    if width > 7l then v else
    Int64.logand v (Int64.lognot (Int64.shift_left (-1L) ((Int32.to_int width) * 8)))

let get_rom (p: prog) (addr: int64) (width: int32): int64 =
    let v_full = p.rom addr in
    cut_width v_full width

let sext (v: int64) (in_width: int32) (out_width: int32): int64 =
    let x = Int64.shift_left v (64 - (Int32.to_int in_width) * 8) in
    let x = Int64.shift_right x (64 - (Int32.to_int in_width) * 8) in
    cut_width x out_width