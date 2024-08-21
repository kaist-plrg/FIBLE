open Common
open Basic_domain
open Value_domain
open Syn
module Map = KeyMap.Make (AbsNumeric)

type t = Map.t

let le = Map.le
let top = Map.top
let join a b = Map.join a b
let meet a b = Map.meet a b
let clear_memref a = Map.clear_memref a
let clear_tempreg a = Map.clear_tempreg a
let clear_mr a (r : RegId.t_full) = Map.clear_mr a r
let pp = Map.pp
let find_opt = Map.find_opt

let eval_varnode (a : t) (d : OctagonD.t) (vn : VarNode.t) =
  match vn with
  | Const c -> AbsNumeric.of_const c.value
  | Register r -> (
      match
        Map.find_filter_opt
          (function
            | KReg r' ->
                RegId.compare r'.id r.id = 0
                && Int32.compare r'.offset r.offset = 0
            | _ -> false)
          a
      with
      | None -> AbsNumeric.of_interval (OctagonD.request_interval d r)
      | Some (_, v) ->
          AbsNumeric.meet
            (AbsNumeric.of_interval (OctagonD.request_interval d r))
            v)
  | Ram _ -> AbsNumeric.top

let process_load (rom : DMem.t) (a : t) (d : OctagonD.t) (outv : RegId.t_full)
    (addrSet : AExprSet.t) =
  let cv =
    AExprSet.fold
      (fun ae o ->
        match o with
        | None -> (
            match ae with
            | { base; offset } when Z.equal offset Z.zero ->
                Map.find_opt (KReg base) a
            | _ -> None)
        | _ -> o)
      addrSet None
  in
  match Option.bind cv (fun x -> AbsNumeric.try_concretize x) with
  | Some vset ->
      Map.add (KReg outv)
        (AbsNumeric.of_limset
           (LimSetD.LimSet
              (Int64Set.map
                 (fun x ->
                   Sem.Value.value_64
                     (DMem.get_numeric rom x (RegId.width outv))
                   |> Result.get_ok)
                 vset)))
        a
  | None -> clear_mr a outv

let process_assignment (a : t) (d : OctagonD.t) (asn : Assignable.t)
    (outv : RegId.t_full) =
  let na = clear_mr a outv in
  match asn with
  | Avar vn -> Map.add (KReg outv) (eval_varnode a d vn) na
  | Abop (Bint_add, op1v, op2v) ->
      let vn1 = eval_varnode a d op1v in
      let vn2 = eval_varnode a d op2v in
      Map.add (KReg outv) (AbsNumeric.add vn1 vn2 (RegId.width outv)) na
  | Abop (Bint_sub, op1v, op2v) ->
      let vn1 = eval_varnode a d op1v in
      let vn2 = eval_varnode a d op2v in
      Map.add (KReg outv) (AbsNumeric.sub vn1 vn2 (RegId.width outv)) na
  | Abop (Bint_mult, op1v, op2v) ->
      let vn1 = eval_varnode a d op1v in
      let vn2 = eval_varnode a d op2v in
      Map.add (KReg outv) (AbsNumeric.mul vn1 vn2 (RegId.width outv)) na
  | Abop (_, _, _) -> na
  | Auop (Uint_sext, vn) ->
      let v = eval_varnode a d vn in
      Map.add (KReg outv)
        (AbsNumeric.sext v (VarNode.get_width vn) (RegId.width outv))
        na
  | Auop (Uint_zext, vn) -> Map.add (KReg outv) (eval_varnode a d vn) na
  | Auop (_, _) -> na

let process_store (a : t) (d : OctagonD.t) (vn : VarNode.t)
    (addrSet : AExprSet.t) : t =
  a
