Require Import Coq.NArith.NArith.
Require Import Coq.Strings.String.
Require Import Coq.Lists.List.
Require Import ExtLib.Structures.Monads.
Require Import ExtLib.Structures.Foldable.
Require Import ExtLib.Structures.Reducible.
Require Import ExtLib.Data.Monads.EitherMonad.
Require Import ExtLib.Data.Monads.OptionMonad.
Require Import ExtLib.Data.List.
From Core Require Import PCodeSyntax.
From bbv Require Import Word.

Open Scope monad_scope.

Definition mem: Set := addr -> word 8.

Record regs: Set := {
  ureg: N -> word 64;
  dreg: N -> word 64
}.

Record state: Set := {
  registers: regs;
  memory: mem;
  pc: word 64;
  seqnum: nat
}.

Definition init_state (p: prog): state :=
  {|
    registers := {|
      ureg := fun _ => wzero 64;
      dreg := fun _ => wzero 64 |};
    memory := fun _ => wzero 8;
    pc := wzero 64;
    seqnum := 0
  |}.

Definition set_pc (pc' : word 64) (s: state): state :=
  match s with
  | {| registers := r; memory := m; pc := _; seqnum := _ |} =>
    {| registers := r; memory := m; pc := pc'; seqnum := 0 |}
  end.

Definition add_pc (count: word 64) (s: state):
state :=
  match s with
  | {| registers := r; memory := m; pc := p; seqnum := _ |} =>
    {| registers := r; memory := m; pc := wplus p count; seqnum := 0 |}
  end.

Definition set_pc_0 (s: state): state :=
  set_pc (wzero 64) s.

Definition set_reg (vn: varNode) (v: word 64) (s: state): state :=
  match vn.(varNode_node) with
  | Unique n => 
    match s with
    | {| registers := r; memory := m; pc := p; seqnum := sn |} =>
      {| registers := {|
        ureg := fun n' => if N.eq_dec n n' then v else r.(ureg) n';
        dreg := r.(dreg) |};
        memory := m; pc := p; seqnum := sn |}
    end
  | Register n =>
    match s with
    | {| registers := r; memory := m; pc := p; seqnum := sn |} =>
      {| registers := {|
        ureg := r.(ureg);
        dreg := fun n' => if N.eq_dec n n' then v else r.(dreg) n' |};
        memory := m; pc := p; seqnum := sn |}
    end
  | Const _ => s
  end.

Definition eval_vnode (vn: varNode) (s: state): word 64 :=
  match vn.(varNode_node) with
  | Unique n => s.(registers).(ureg) n
  | Register n => s.(registers).(dreg) n
  | Const n => n
end.

(*
Inductive uop: Set :=
| Upopcount: uop | Ulzcount: uop | Uint_zext: uop | Uint_sext: uop
| Uint_2comp: uop | Uint_negate: uop | Ubool_negate: uop | Ufloat_neg: uop
| Ufloat_abs: uop | Ufloat_sqrt: uop | Ufloat_ceil: uop
| Ufloat_floor: uop | Ufloat_round: uop | Ufloat_nan: uop
| Uint2float: uop | Ufloat2float: uop | Utrunc: uop
.
*)
Definition eval_uop (op: uop) (width: N) (v: word 64): word 64 := wzero 64.

Definition eval_bop (op: bop) (width: N) (v0 v1: word 64): word 64 := wzero 64.

Definition eval_ae (ae: assignable) (width: N) (s: state): word 64 :=
  match ae with
  | Avar input => eval_vnode input s
  | Auop op input => eval_uop op width (eval_vnode input s)
  | Abop op input0 input1 => eval_bop op width (eval_vnode input0 s) (eval_vnode input1 s)
  end.

Definition step_op (op: inst) (s: state): sum state state :=
  match op with
  | Ijump _ vn => raise (set_pc (eval_vnode vn s) s)
  | Ijump_ind _ vn => raise (set_pc (eval_vnode vn s) s)
  | Icbranch cn jn => raise s
  | Iassignment ae vn => ret (set_reg vn (eval_ae ae vn.(varNode_width) s) s)
  | Iload _ _ _ => ret s
  | Istore _ _ _ => ret s
  | Iunimplemented => ret (set_pc_0 s)
  end.

Definition step_ops (ops: list inst) (s: state): sum state state :=
  foldM step_op (ret s) ops.

Definition merge_sum {A: Type} (s: sum A A) : A :=
  match s with
  | inl a => a
  | inr a => a
  end.

Definition reposition (vs: (N * list inst)) (s: state): state :=
 match vs with
 | (v, is) => if Nat.ltb s.(seqnum) (length is) then add_pc (NToWord 64 v) s else s
 end.

Definition apply_op (vs: (N * list inst)) (s: state): option state :=
  match vs with
  | (v, is) =>
   liftM (fun i =>
     (merge_sum (liftM (reposition (v, is)) (step_op i s)))
    ) (nth_error is (s.(seqnum)))
  end.

Definition step (p: prog) (s: state): option state :=
   bind (p.(ins_mem) s.(pc)) (fun vs => apply_op vs s).

Close Scope monad_scope.