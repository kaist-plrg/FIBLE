open StdlibExt
open Basic
open Basic_domain
open Value_domain
include MemRefTopMapD.Make (AbsVal)

let clear_memref a =
  filter
    (fun k _ ->
      match k with
      | MemRef.ROffsetR _ -> false
      | MemRef.UOffsetR _ -> false
      | _ -> true)
    a

let clear_mr a (mr : MemRef.t) = filter (fun k _ -> Stdlib.compare k mr <> 0) a

let eval_varnode (a : t) (d : OctagonD.t) (vn : VarNode.t) =
  match vn.varNode_node with
  | Const c -> AbsVal.of_const c
  | Unique u -> (
      match find_opt (MemRef.UniqueR u) a with
      | None ->
          AbsVal.of_interval (OctagonD.request_interval d (MemRef.UniqueR u))
      | Some v ->
          AbsVal.meet
            (AbsVal.of_interval
               (OctagonD.request_interval d (MemRef.UniqueR u)))
            v)
  | Register r -> (
      match find_opt (MemRef.RegisterR r) a with
      | None ->
          AbsVal.of_interval (OctagonD.request_interval d (MemRef.RegisterR r))
      | Some v ->
          AbsVal.meet
            (AbsVal.of_interval
               (OctagonD.request_interval d (MemRef.RegisterR r)))
            v)
  | Ram _ -> AbsVal.top

let process_load (p : Prog.t) (a : t) (d : OctagonD.t) (pointerv : VarNode.t)
    (outv : VarNode.t) =
  match (pointerv.varNode_node, MemRef.convert_varnode outv) with
  | Unique u, Some outmr -> (
      match
        Option.bind (find_opt (MemRef.UniqueR u) a) (fun x ->
            AbsVal.try_concretize x 20)
      with
      | Some vset ->
          add outmr
            (AbsVal.of_limset
               (LimSetD.LimSet
                  (Int64Set.map
                     (fun x -> Prog.get_rom p x outv.varNode_width)
                     vset)))
            a
      | None ->
          add outmr
            (AbsVal.of_interval
               (OctagonD.request_interval d (MemRef.UniqueR u)))
            a)
  | _, Some outmr -> clear_mr a outmr
  | _ -> a

let process_assignment (a : t) (d : OctagonD.t) (asn : Assignable.t)
    (outv : VarNode.t) =
  match MemRef.convert_varnode outv with
  | None -> a
  | Some outmr -> (
      let na = clear_mr a outmr in
      match asn with
      | Avar vn -> add outmr (eval_varnode a d vn) na
      | Abop (Bint_add, op1v, op2v) ->
          let vn1 = eval_varnode a d op1v in
          let vn2 = eval_varnode a d op2v in
          add outmr (AbsVal.add vn1 vn2 outv.varNode_width) na
      | Abop (Bint_sub, op1v, op2v) ->
          let vn1 = eval_varnode a d op1v in
          let vn2 = eval_varnode a d op2v in
          add outmr (AbsVal.sub vn1 vn2 outv.varNode_width) na
      | Abop (Bint_mult, op1v, op2v) ->
          let vn1 = eval_varnode a d op1v in
          let vn2 = eval_varnode a d op2v in
          add outmr (AbsVal.mul vn1 vn2 outv.varNode_width) na
      | Abop (_, _, _) -> na
      | Auop (Uint_sext, vn) ->
          let v = eval_varnode a d vn in
          add outmr (AbsVal.sext v vn.varNode_width outv.varNode_width) na
      | Auop (Uint_zext, vn) -> add outmr (eval_varnode a d vn) na
      | Auop (_, _) -> na)
