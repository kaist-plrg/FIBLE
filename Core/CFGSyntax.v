Require Import Coq.NArith.NArith.
From Core Require Import PCodeSyntax.

Inductive inst: Set :=
| Iload (input0: varNode) (input1: varNode) (output: varNode): inst
| Istore (input0: varNode) (input1: varNode) (input2: varNode): inst
| Iassignment (input: assignable) (output: varNode): inst
.

Inductive jump: Set :=
| Jswitch (input: varNode) (locs: list N): jump
| Jcbranch (input: varNode) (ltrue: N) (lfalse: N): jump
| Jbranch (l: N): jump
| Jcall (target: varNode) (lret: N): jump
| Jreturn: jump
.

Definition block: Set := N * (list inst) * jump.

Definition func: Set := block * (list block).

Definition prog: Set := func * (list func).