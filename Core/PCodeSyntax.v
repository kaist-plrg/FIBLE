Require Import Coq.NArith.NArith.
Require Import Coq.Strings.String.
From bbv Require Import Word.

Inductive varNodeI: Set :=
 | Unique (n: N): varNodeI
 | Register (n: N): varNodeI
 | Const (n: word 64): varNodeI
.

Record varNode: Set := {
    varNode_node: varNodeI;
    varNode_width: N
}.

Inductive bop: Set :=
 | Bpiece: bop | Bsubpiece: bop | Bint_equal: bop | Bint_notequal: bop
 | Bint_less: bop | Bint_sless: bop | Bint_lessequal: bop | Bint_slessequal: bop
 | Bint_add: bop | Bint_sub: bop | Bint_carry: bop | Bint_scarry: bop
 | Bint_sborrow: bop | Bint_xor: bop | Bint_and: bop | Bint_or: bop
 | Bint_left: bop | Bint_right: bop | Bint_sright: bop | Bint_mult: bop
 | Bint_div: bop | Bint_rem: bop | Bint_sdiv: bop | Bint_srem: bop
 | Bbool_xor: bop | Bbool_and: bop | Bbool_or: bop | Bfloat_equal: bop
 | Bfloat_notequal: bop | Bfloat_less: bop | Bfloat_lessequal: bop
 | Bfloat_add: bop | Bfloat_sub: bop | Bfloat_mult: bop | Bfloat_div: bop
.

Inductive uop: Set :=
 | Upopcount: uop | Ulzcount: uop | Uint_zext: uop | Uint_sext: uop
 | Uint_2comp: uop | Uint_negate: uop | Ubool_negate: uop | Ufloat_neg: uop
 | Ufloat_abs: uop | Ufloat_sqrt: uop | Ufloat_ceil: uop
 | Ufloat_floor: uop | Ufloat_round: uop | Ufloat_nan: uop
 | Uint2float: uop | Ufloat2float: uop | Utrunc: uop
.

Inductive assignable: Set :=
 | Avar (input: varNode): assignable
 | Auop (op: uop) (input: varNode): assignable
 | Abop (op: bop) (input0: varNode) (input1: varNode): assignable
.

Inductive jannotation: Set :=
 | Jbranch: jannotation
 | Jcall: jannotation
.

Inductive jiannotation: Set :=
 | JIbranch: jiannotation
 | JIcall: jiannotation
 | JIret: jiannotation
.

Inductive inst: Set :=
 | Iunimplemented: inst
 | Iload (input0: varNode) (input1: varNode) (output: varNode): inst
 | Istore (input0: varNode) (input1: varNode) (input2: varNode): inst
 | Ijump (a: jannotation) (input: varNode): inst
 | Ijump_ind (a: jiannotation) (input: varNode): inst
 | Icbranch (input0: varNode) (input1: varNode): inst
 | Iassignment (input: assignable) (output: varNode): inst
.

Definition addr: Set := word 64.

Definition loc: Set := addr * nat.

Record prog: Set := {
    ins_mem: addr -> option (N * list inst);
    entry_addr: addr
}.
