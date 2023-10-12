open Basic
open Basic_domain
open Value_domain
include VarNodeTopMapD.MakeLatticeWithTop (SPVal)


let eval_varnode (a : t) (vn : VarNode.t) =
  match vn.varNode_node with
  | Const c -> { SPVal.have_sp = FlatBoolD.Flat false; SPVal.offset = FlatInt64D.Flat c }
  | Unique u ->
      Option.value (find_opt vn a) ~default:SPVal.bottom
  | Register r ->
      Option.value (find_opt vn a) ~default:SPVal.bottom
  | Ram _ -> SPVal.top

let process_assignment (a : t) (asn : Assignable.t)
    (outv : VarNode.t) =
      match asn with
      | Avar vn -> add outv (eval_varnode a vn) a
      | Abop (Bint_add, op1v, op2v) ->
          let vn1 = eval_varnode a op1v in
          let vn2 = eval_varnode a op2v in
          add outv (SPVal.add vn1 vn2 outv.varNode_width) a
      | Abop (Bint_sub, op1v, op2v) ->
          let vn1 = eval_varnode a op1v in
          let vn2 = eval_varnode a op2v in
          add outv (SPVal.sub vn1 vn2 outv.varNode_width) a
      | Abop (Bint_mult, op1v, op2v) ->
          let vn1 = eval_varnode a op1v in
          let vn2 = eval_varnode a op2v in
          add outv (SPVal.mul vn1 vn2 outv.varNode_width) a
      | Abop (_, _, _) -> a
      | Auop (Uint_sext, vn) ->
          let v = eval_varnode a vn in
          add outv (SPVal.sext v vn.varNode_width outv.varNode_width) a
      | Auop (Uint_zext, vn) -> add outv (eval_varnode a vn) a
      | Auop (_, _) -> a
