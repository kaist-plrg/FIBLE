Require Import Coq.NArith.NArith.
Require Import ExtLib.Core.RelDec.
Require Import ExtLib.Structures.Sets.
Require Import ExtLib.Structures.Maps.
Require Import ExtLib.Structures.Monads.
Require Import ExtLib.Structures.Foldable.
Require Import ExtLib.Structures.Reducible.
Require Import ExtLib.Data.Monads.EitherMonad.
Require Import ExtLib.Data.Monads.OptionMonad.
Require Import ExtLib.Data.List.
Require Import ExtLib.Data.Map.FMapAList.
Require Import ExtLib.Data.Set.ListSet.

From bbv Require Import Word.
From Core Require Import PCodeSyntax.

(* mainly follows Ghidra/Features/Decompiler/src/decompile/cpp/funcdata_op.cc:711
void Funcdata::followFlow(const Address &baddr,const Address &eaddr)

flow.generateOps();
flow.generateBlocks();
switchOverJumpTables(flow);


generateOps() {
  addrlist.push_back(data.getAddress());
  while(!addrlist.empty())	// Recovering as much as possible except jumptables
    fallthru();

    // Recovering jumptables
    while(!tablelist.empty()) {	// For each jumptable found
      vector<JumpTable *> newTables;
      recoverJumpTables(newTables, notreached);
            for(int4 i=0;i<newTables.size();++i) {
                JumpTable *jt = newTables[i];
                int4 num = jt->numEntries();
                for(int4 i=0;i<num;++i)
                    newAddress(jt->getIndirectOp(),jt->getAddressByIndex(i));
                    while(!addrlist.empty())	// Try to fill in as much more as possible
	                    fallthru();
                }
}
*)


Definition get_or_else {A: Type} (o: option A) (d: A): A :=
  match o with
  | Some a => a
  | None => d
  end.

Axiom OcamlMap: Type -> Type -> Set.
Axiom OcamlMap_merge: forall K V: Type, (V -> V -> V) -> OcamlMap K V -> OcamlMap K V -> OcamlMap K V.
Axiom OcamlMapIsMap: forall K V: Type, Map K V (OcamlMap K V).
Global Instance InstanceOcamlMap {K V: Type}: Map K V (OcamlMap K V) := OcamlMapIsMap K V.

Axiom OcamlSet: Type -> Set.
Axiom OcamlSet_map: forall A B: Type, (A -> B) -> OcamlSet A -> OcamlSet B.
Axiom OcamlSetIsSet: forall V: Type, DSet (OcamlSet V) V.
Global Instance InstanceOcamlSet {V: Type}: DSet (OcamlSet V) V := OcamlSetIsSet V.

(* 1: fall-trough: P-code is generated for instructions starting at this address until one no longer has fall-thru flow*)

Global Instance loc_RelDec: RelDec (@eq loc) := {
  rel_dec a b := andb (Word.weqb (fst a) (fst b))
   (Nat.eqb (snd a) (snd b))
}.

Record astate_domain: Type := {
  elem_type: Type;
  elem_top: elem_type;
}.

Section CFA.
Variable astate: astate_domain.


Record contour: Set := {
  unsound_jump: OcamlMap loc (option (OcamlSet loc));
  basic_block: OcamlSet loc * OcamlSet loc;
  sound_jump: OcamlMap loc (OcamlSet loc);
}.

Record cfg_astate: Set := {
  analysis_contour: contour;
  abstract_state: OcamlMap loc astate.(elem_type);
}.

Definition et := astate.(elem_type).
Definition atop: et := astate.(elem_top).


Local Instance map_uj: Map loc (option (OcamlSet loc)) (OcamlMap loc (option (OcamlSet loc))) := OcamlMapIsMap _ _.
Local Instance set_bb: DSet (OcamlSet loc) loc := OcamlSetIsSet _.
Local Instance map_sj: Map loc (OcamlSet loc) (OcamlMap loc (OcamlSet loc)) := OcamlMapIsMap _ _.
Local Instance map_as: Map loc et (OcamlMap loc et) := OcamlMapIsMap _ _.

Definition odset_join (a b: option (OcamlSet loc)): option (OcamlSet loc) :=
  match a, b with
  | Some a, Some b => Some (Sets.union a b)
  | Some a, None => Some a
  | None, Some b => Some b
  | None, None => None
  end.

Definition astate_join (a b: et): et := a.

Definition join_contour (a: contour) (b: contour) :=
    let u := OcamlMap_merge _ _ (odset_join) (unsound_jump a) (unsound_jump b) in
    let s := OcamlMap_merge _ _ (@Sets.union _ _ set_bb) (sound_jump a) (sound_jump b) in
    let ents := @Sets.union _ _ set_bb (fst (basic_block a)) (fst (basic_block b)) in
    let exts := @Sets.union _ _ set_bb (snd (basic_block a)) (snd (basic_block b)) in
    {| unsound_jump := u; basic_block := (ents, exts); sound_jump := s |}.
  
Definition join_cfg (a: cfg_astate) (b: cfg_astate) :=
    let c := join_contour (analysis_contour a) (analysis_contour b) in
    let a := OcamlMap_merge _ _ astate_join (abstract_state a) (abstract_state b) in
    {| analysis_contour := c; abstract_state := a |}.

Inductive heuristic_result: Set :=
 | HrUnsound: (OcamlSet loc) -> heuristic_result
 | HrSound: (OcamlSet loc) -> heuristic_result
 | HrExit: heuristic_result
 | HrFallthrough: heuristic_result.

Axiom flow_heuristic: prog -> loc -> et -> inst -> heuristic_result.

Definition reposition_sn (vs: (N * list inst)) (s: loc): loc :=
 match vs with
 | (v, is) => if Nat.ltb (snd s) (length is) then s else (wplus (fst s) (NToWord 64 v), 0) 
 end.

Definition reposition_sn_set (vs: (N * list inst)) (s: OcamlSet loc): OcamlSet loc :=
  OcamlSet_map _ _ (reposition_sn vs) s.

Definition fallthru (p: prog) (l: loc): loc :=
 let nl := (fst l, S (snd l)) in
 match p.(ins_mem) (fst l) with
  | Some vs => reposition_sn vs nl
  | None => nl
 end.

Definition gen_contour_single (p: prog) (l: loc) (a: et) (i: inst): contour :=
  match flow_heuristic p l a i with
  | HrUnsound s => {| unsound_jump := Maps.singleton l (Some s); basic_block := (Sets.empty, Sets.empty); sound_jump := Maps.empty |}
  | HrSound s => {| unsound_jump := Maps.empty; basic_block := (Sets.empty, Sets.empty); sound_jump := Maps.singleton l s |}
  | HrExit => {| unsound_jump := Maps.empty; basic_block := (Sets.empty, Sets.singleton l); sound_jump := Maps.empty |}
  | HrFallthrough => {| unsound_jump := Maps.empty; basic_block := (Sets.singleton (fallthru p l), Sets.singleton l); sound_jump := Maps.empty |}
  end.

Definition flow_heuristic_simple (p: prog) (l: loc) (a: et) (i: inst): heuristic_result := match i with
| Ijump _ vn => HrUnsound (Sets.empty)
| Ijump_ind _ vn => HrUnsound (Sets.empty)
| Icbranch cn jn => HrUnsound (Sets.empty)
| Iunimplemented => HrUnsound (Sets.empty)
| _ => HrSound (Sets.singleton (fallthru p l))
end.

Definition post_contour_single (p: prog) (ls lf: loc) (a: et) (i: inst): et := atop.

Definition follow_ops (vs: (N * list inst)) (a: addr) (sn: nat): option cfg_astate :=
  match vs with
  | (v, is) =>
   liftM (fun i =>
      (divide_set_sum (liftM (reposition_sn_set (v, is)) (follow_op i a sn)))
    ) (nth_error is sn)
  end.

Definition astep' (p: prog) (a: addr) (sn: nat): cfg_astate := get_or_else (bind (p.(ins_mem) a) (fun vs => follow_ops vs a sn)) {| unsound_jump := empty; sound_jump := empty |}.


Definition astep (p: prog) (s: cfg_astate): cfg_astate :=
   join_cfg s (fold_left (fun s a => join_cfg s (astep' p (fst a) (snd a))) (sound_jump s) {| unsound_jump := empty; sound_jump := empty |}).

Definition astep_init (a: addr): cfg_astate :=
  {| unsound_jump := empty; sound_jump := singleton (a, 0) |}.

Definition astep_n (p: prog) (n: nat) (s: cfg_astate): cfg_astate :=
    fold_left (fun s _ => astep p s) (seq 0 n) s.

Definition follow_flow (p: prog) (a: addr): cfg_astate :=
    astep_n p 1000 (astep_init a).

End CFA.