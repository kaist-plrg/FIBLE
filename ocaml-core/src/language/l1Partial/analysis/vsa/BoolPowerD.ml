open Basic
open Basic_domain

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

let clear_mr a (r : RegId.t) =
  Map.clear_mr a r
  |> Map.map (fun (vt, vf) -> (OctagonD.clear_mr vt r, OctagonD.clear_mr vf r))

let process_assignment (a : t) (d : OctagonD.t) (asn : Assignable.t)
    (outv : RegId.t_full) =
  let outmr : Key.t = KReg outv.id in
  let a =
    Map.map
      (fun (vt, vf) ->
        ( OctagonD.process_assignment vt asn outv,
          OctagonD.process_assignment vf asn outv ))
      a
  in
  (* NOTE: should use Map.clear_mr to not delete outv.id inside of map *)
  let na = Map.clear_mr a outv.id in
  match asn with
  | Avar (Register r) ->
      Map.find_opt (KReg r.id) na
      |> Option.map (fun v -> Map.add outmr v na)
      |> Option.value ~default:na
  | Avar _ -> na
  | Abop (Bint_less, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r, Const { value = c; _ } ->
          (Map.add outmr
             ( OctagonD.gen_single_lt (OctagonD.gen_single_ge d r.id 0L) r.id c,
               OctagonD.gen_single_ge d r.id c ))
            na
      | _ -> na)
  | Abop (Bint_equal, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r, Const { value = c; _ } ->
          (Map.add outmr (OctagonD.gen_single_eq d r.id c, d)) na
      | _ -> na)
  | Abop (Bint_sless, _, _) -> na
  | Abop (Bint_slessequal, _, _) -> na
  | Abop (Bbool_or, op1v, op2v) -> (
      match (op1v, op2v) with
      | Register r1, Register r2 -> (
          match
            (Map.find_opt (KReg r1.id) na, Map.find_opt (KReg r2.id) na)
          with
          | Some (dt, df), Some (dt2, df2) ->
              Map.add outmr (OctagonD.join dt dt2, OctagonD.meet df df2) na
          | _ -> na)
      | _ -> na)
  | Abop (Bint_add, Register r, Const _) ->
      if RegId.compare outv.id r.id = 0 then a else na
  | Abop (Bint_sub, Register r, Const _) ->
      if RegId.compare outv.id r.id = 0 then a else na
  | Abop (_, _, _) -> na
  | Auop (Ubool_negate, opv) -> (
      match opv with
      | Register r -> (
          match Map.find_opt (KReg r.id) na with
          | Some (d1, d2) -> Map.add outmr (d2, d1) na
          | _ -> na)
      | _ -> na)
  | Auop (_, _) -> na

let process_load (rom : Addr.t -> Char.t) (a : t) (d : OctagonD.t)
    (outv : RegId.t_full) (addrSet : AExprSet.t) : t =
  clear_mr a outv.id
  |> Map.map (fun (vt, vf) ->
         ( OctagonD.process_load rom vt outv addrSet,
           OctagonD.process_load rom vf outv addrSet ))

let process_store (a : t) (d : OctagonD.t) (vn : VarNode.t)
    (addrSet : AExprSet.t) : t =
  Map.map
    (fun (vt, vf) ->
      ( OctagonD.process_store vt vn addrSet,
        OctagonD.process_store vf vn addrSet ))
    a
