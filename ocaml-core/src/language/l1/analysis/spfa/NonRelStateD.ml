open Common
open Basic_domain
open Value_domain
include RegIdTopMapD.MakeLatticeWithTop (AbsVal)

let eval_varnode (a : t) (vn : VarNode.t) =
  match vn with
  | Const { value = c; _ } ->
      {
        AbsVal.have_sp = FlatBoolD.Flat false;
        AbsVal.offset = FlatInt64D.Flat c;
      }
  | Register r -> Option.value (find_opt r.id a) ~default:AbsVal.bottom
  | Ram v -> AbsVal.top

let process_assignment (a : t) (asn : Assignable.t) (outv : RegId.t_full) =
  match asn with
  | Avar vn -> add outv.id (eval_varnode a vn) a
  | Abop (Bint_add, op1v, op2v) ->
      let vn1 = eval_varnode a op1v in
      let vn2 = eval_varnode a op2v in
      add outv.id (AbsVal.add vn1 vn2 (RegId.width outv)) a
  | Abop (Bint_sub, op1v, op2v) ->
      let vn1 = eval_varnode a op1v in
      let vn2 = eval_varnode a op2v in
      add outv.id (AbsVal.sub vn1 vn2 (RegId.width outv)) a
  | Abop (Bint_mult, op1v, op2v) ->
      let vn1 = eval_varnode a op1v in
      let vn2 = eval_varnode a op2v in
      add outv.id (AbsVal.mul vn1 vn2 (RegId.width outv)) a
  | Abop (_, _, _) -> a
  | Auop (Uint_sext, vn) ->
      let v = eval_varnode a vn in
      add outv.id (AbsVal.sext v (VarNode.width vn) (RegId.width outv)) a
  | Auop (Uint_zext, vn) -> add outv.id (eval_varnode a vn) a
  | Auop (_, _) -> a
