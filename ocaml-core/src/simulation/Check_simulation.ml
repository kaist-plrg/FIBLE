open StdlibExt
open Notation
open Basic

let check_pc (s0 : Loc.t) (s1 : L1.Sem.Cont.t) (s2 : L2Partial.Sem.Cont.t) :
    (Unit.t, String.t) Result.t =
  let s1_pc = match s1.remaining with [] -> s1.jmp.loc | s :: _ -> s.loc in
  let s2_pc = match s2.remaining with [] -> s2.jmp.loc | s :: _ -> s.loc in
  if Loc.compare s0 s1_pc = 0 && Loc.compare s1_pc s2_pc = 0 then Ok ()
  else Error "PCs are not equal"

let check_simu_state (s0 : L0.Sem.State.t) (s1 : L1.Sem.State.t)
    (s2 : L2Partial.Sem.State.t) : (Unit.t, String.t) Result.t =
  let* _ = check_pc s0.pc s1.cont s2.cont in
  Ok ()

let collect_errors (nl0 : (L0.Sem.State.t, StopEvent.t) Result.t)
    (nl1 : (L1.Sem.State.t, StopEvent.t) Result.t)
    (nl2 : (L2Partial.Sem.State.t, StopEvent.t) Result.t) : String.t =
  let l0_err = match nl0 with Ok _ -> "" | Error e -> "" in
  let l1_err = match nl1 with Ok _ -> "" | Error e -> "" in
  let l2_err = match nl2 with Ok _ -> "" | Error e -> "" in
  String.concat "\n" [ "L0: " ^ l0_err; "L1: " ^ l1_err; "L2: " ^ l2_err ]

let run (rspec : Int32.t Int32Map.t) (l0 : L0.Prog.t) (l1 : L1.Prog.t)
    (l2 : L2Partial.Prog.t) (addr : Addr.t) : (Unit.t, String.t) Result.t =
  let l0_state = L0.Init.from_signature l0 addr in
  let l1_state = L1.Init.from_signature l1 addr in
  let l2_state = L2Partial.Init.from_signature l2 addr in
  let rec aux (l0_state : L0.Sem.State.t) (l1_state : L1.Sem.State.t)
      (l2_state : L2Partial.Sem.State.t) : (Unit.t, String.t) Result.t =
    let* _ = check_simu_state l0_state l1_state l2_state in
    let nl0 = L0.Interp.step l0 l0_state in
    let nl1 = L1.Interp.step l1 l1_state in
    let nl2 = L2Partial.Interp.step l2 l2_state in
    match (nl0, nl1, nl2) with
    | Ok l0_state, Ok l1_state, Ok l2_state -> aux l0_state l1_state l2_state
    | _ -> Error (collect_errors nl0 nl1 nl2)
  in
  aux l0_state l1_state l2_state
