open StdlibExt
open Basic
open Basic_domain
open Value_domain
include MemRefTopMapD.Make (AbsVal)

let clear_memref a =
  filter (fun k _ -> match k with MemRef.ROffset _ -> false | _ -> true) a

let clear_mr a (mr : MemRef.t) = filter (fun k _ -> Stdlib.compare k mr <> 0) a

let eval_varnode (a : t) (d : OctagonD.t) (vn : VarNode.t) =
  match vn with
  | Const c -> AbsVal.of_const c.value
  | Register r -> (
      match find_opt (MemRef.R r) a with
      | None -> AbsVal.of_interval (OctagonD.request_interval d (MemRef.R r))
      | Some v ->
          AbsVal.meet
            (AbsVal.of_interval (OctagonD.request_interval d (MemRef.R r)))
            v)

let process_load (p : L0.Prog.t) (a : t) (d : OctagonD.t) (pointerv : VarNode.t)
    (outv : RegId.t) =
  match (pointerv, MemRef.convert_regid outv) with
  | Register ({ id = RegId.Unique _; _ } as u), outmr -> (
      match
        Option.bind (find_opt (MemRef.R u) a) (fun x ->
            AbsVal.try_concretize x 20)
      with
      | Some vset ->
          add outmr
            (AbsVal.of_limset
               (LimSetD.LimSet
                  (Int64Set.map
                     (fun x -> L0.Prog.get_rom p x (RegId.width outv))
                     vset)))
            a
      | None ->
          add outmr
            (AbsVal.of_interval (OctagonD.request_interval d (MemRef.R u)))
            a)
  | _, outmr -> clear_mr a outmr

let process_assignment (a : t) (d : OctagonD.t) (asn : Assignable.t)
    (outv : RegId.t) =
  match MemRef.convert_regid outv with
  | outmr -> (
      let na = clear_mr a outmr in
      match asn with
      | Avar vn -> add outmr (eval_varnode a d vn) na
      | Abop (Bint_add, op1v, op2v) ->
          let vn1 = eval_varnode a d op1v in
          let vn2 = eval_varnode a d op2v in
          add outmr (AbsVal.add vn1 vn2 (RegId.width outv)) na
      | Abop (Bint_sub, op1v, op2v) ->
          let vn1 = eval_varnode a d op1v in
          let vn2 = eval_varnode a d op2v in
          add outmr (AbsVal.sub vn1 vn2 (RegId.width outv)) na
      | Abop (Bint_mult, op1v, op2v) ->
          let vn1 = eval_varnode a d op1v in
          let vn2 = eval_varnode a d op2v in
          add outmr (AbsVal.mul vn1 vn2 (RegId.width outv)) na
      | Abop (_, _, _) -> na
      | Auop (Uint_sext, vn) ->
          let v = eval_varnode a d vn in
          add outmr (AbsVal.sext v (VarNode.width vn) (RegId.width outv)) na
      | Auop (Uint_zext, vn) -> add outmr (eval_varnode a d vn) na
      | Auop (_, _) -> na)
