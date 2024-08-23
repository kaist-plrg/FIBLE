open Common
open Basic_domain
open Syn

module A = struct
  include TupleD.MakeJoinSemiLatticeWithTop (OctagonD) (OctagonD)

  let pp fmt (oct, ocf) =
    Format.fprintf fmt "(%a, %a)" OctagonD.pp oct OctagonD.pp ocf

  let meet (oct1, ocf1) (oct2, ocf2) =
    (OctagonD.meet oct1 oct2, OctagonD.meet ocf1 ocf2)
end

module Map = KeyMap.Make (A)

type t = Map.t

let le = Map.le
let top = Map.top
let join a b = Map.join a b
let meet a b = Map.meet a b
let pp = Map.pp
let find_opt = Map.find_opt

let clear_memref a =
  Map.clear_memref a
  |> Map.map (fun (vt, vf) ->
         (OctagonD.clear_memref vt, OctagonD.clear_memref vf))

let clear_tempreg a =
  Map.clear_tempreg a
  |> Map.map (fun (vt, vf) ->
         (OctagonD.clear_tempreg vt, OctagonD.clear_tempreg vf))

let clear_mr a (r : RegId.t_full) =
  Map.clear_mr a r
  |> Map.map (fun (vt, vf) -> (OctagonD.clear_mr vt r, OctagonD.clear_mr vf r))

let process_assignment (a : t) (d : OctagonD.t) (asn : Assignable.t)
    (outv : RegId.t_full) =
  let outmr : Key.t = KReg outv in
  let a =
    Map.map
      (fun (vt, vf) ->
        ( OctagonD.process_assignment vt asn outv,
          OctagonD.process_assignment vf asn outv ))
      a
  in
  (* NOTE: should use Map.clear_mr to not delete outv.id inside of map *)
  let na = Map.clear_mr a outv in
  match asn with
  | Avar (Register r) ->
      if RegId.compare_full outv r = 0 then a
      else
        Map.find_opt (KReg r) na
        |> Option.map (fun v -> Map.add outmr v na)
        |> Option.value ~default:na
  | Avar _ -> na
  | Abop (Bint_less, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r, Const { value = c; _ } ->
          let c = Z.of_int64 c in
          (Map.add outmr
             ( OctagonD.gen_single_lt
                 (OctagonD.gen_single_ge d r Z.zero
                    ((r.width |> Int32.to_int) * 8))
                 r c
                 ((r.width |> Int32.to_int) * 8),
               OctagonD.gen_single_ge d r c ((r.width |> Int32.to_int) * 8) ))
            na
      | _ -> na)
  | Abop (Bint_sless, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r, Const { value = c; _ } ->
          let c = Z.of_int64 c in
          (Map.add outmr
             ( OctagonD.gen_single_lt d r c ((r.width |> Int32.to_int) * 8),
               OctagonD.gen_single_ge d r c ((r.width |> Int32.to_int) * 8) ))
            na
      | _ -> na)
  | Abop (Bint_sborrow, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r, Const { value = c; _ } ->
          let c = Z.of_int64 c in
          (Map.add outmr
             ( d,
               OctagonD.gen_single_lt
                 (OctagonD.gen_single_ge d r Z.zero
                    ((r.width |> Int32.to_int) * 8))
                 r c
                 ((r.width |> Int32.to_int) * 8) ))
            na
      | _ -> na)
  | Abop (Bint_lessequal, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r, Const { value = c; _ } ->
          let c = Z.of_int64 c in
          (Map.add outmr
             ( OctagonD.gen_single_lt
                 (OctagonD.gen_single_ge d r Z.zero
                    ((r.width |> Int32.to_int) * 8))
                 r (Z.add c Z.one)
                 ((r.width |> Int32.to_int) * 8),
               OctagonD.gen_single_ge d r (Z.add c Z.one)
                 ((r.width |> Int32.to_int) * 8) ))
            na
      | _ -> na)
  | Abop (Bint_slessequal, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r, Const { value = c; _ } ->
          let c = Z.of_int64 c in
          (Map.add outmr
             ( OctagonD.gen_single_lt d r (Z.add c Z.one)
                 ((r.width |> Int32.to_int) * 8),
               OctagonD.gen_single_ge d r (Z.add c Z.one)
                 ((r.width |> Int32.to_int) * 8) ))
            na
      | _ -> na)
  | Abop (Bint_equal, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r, Const { value = c; _ } ->
          let c = Z.of_int64 c in
          (Map.add outmr
             (OctagonD.gen_single_eq d r c ((r.width |> Int32.to_int) * 8), d))
            na
      | Register r1, Register r2 -> (
          match (Map.find_opt (KReg r1) na, Map.find_opt (KReg r2) na) with
          | Some (dt, df), Some (dt2, df2) ->
              if
                RegId.compare r1.id (Register 0x20bl) = 0
                (* OF *) && RegId.compare r2.id (Register 0x207l) = 0 (* SF *)
              then Map.add outmr (dt, df) na
              else
                Map.add outmr
                  ( OctagonD.join (OctagonD.meet dt dt2) (OctagonD.meet df df2),
                    OctagonD.join (OctagonD.meet dt df2) (OctagonD.meet df dt2)
                  )
                  na
          | _ -> na)
      | _ -> na)
  | Abop (Bbool_and, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r1, Register r2 -> (
          match (Map.find_opt (KReg r1) na, Map.find_opt (KReg r2) na) with
          | Some (dt, df), Some (dt2, df2) ->
              Map.add outmr (OctagonD.meet dt dt2, OctagonD.join df df2) na
          | _ -> na)
      | _ -> na)
  | Abop (Bbool_or, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r1, Register r2 -> (
          match (Map.find_opt (KReg r1) na, Map.find_opt (KReg r2) na) with
          | Some (dt, df), Some (dt2, df2) ->
              let dtn, dfn = (OctagonD.join dt dt2, OctagonD.meet df df2) in
              if
                RegId.compare r1.id (Register 0x200l) = 0
                && RegId.compare r2.id (Register 0x206l) = 0
              then ();
              Map.add outmr (dtn, dfn) na
          | _ -> na)
      | _ -> na)
  | Abop (Bint_add, Register r, Const _) ->
      if RegId.compare_full outv r = 0 then a else na
  | Abop (Bint_sub, Register r, Const _) ->
      if RegId.compare_full outv r = 0 then a else na
  | Abop (_, _, _) -> na
  | Auop (Ubool_negate, opv) -> (
      match opv with
      | Register r -> (
          match Map.find_opt (KReg r) na with
          | Some (d1, d2) -> Map.add outmr (d2, d1) na
          | _ -> na)
      | _ -> na)
  | Auop (_, _) -> na

let process_load (rom : DMem.t) (a : t) (d : OctagonD.t) (outv : RegId.t_full)
    (addrSet : AExprSet.t) : t =
  clear_mr a outv
  |> Map.map (fun (vt, vf) ->
         ( OctagonD.process_load rom vt outv addrSet,
           OctagonD.process_load rom vf outv addrSet ))

let process_store (a : t) (d : OctagonD.t) (vn : Common.NumericVarNode.t)
    (addrSet : AExprSet.t) : t =
  Map.map
    (fun (vt, vf) ->
      ( OctagonD.process_store vt vn addrSet,
        OctagonD.process_store vf vn addrSet ))
    a
