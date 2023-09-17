
type varNodeI =
 | Unique of int64
 | Register of int64
 | Const of int64


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


type addr = int64

type loc = addr * int

type prog = {
    ins_mem: addr -> (int * (inst list)) option;
    entry_addr: addr
}


let follow_flow (p: prog) (e: addr): (addr list * addr list) = ([], [])