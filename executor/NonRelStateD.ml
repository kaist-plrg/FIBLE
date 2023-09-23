open DS;;

type t = AbsVal.t MemRefTopMap.t

let join = MemRefTopMap.mapjoin AbsVal.join
let ole a b = MemRefTopMap.mapole AbsVal.ole AbsVal.top a b

let top = MemRefTopMap.top

let pp fmt a = MemRefTopMap.pp AbsVal.pp fmt a

let clear_memref a = MemRefTopMap.filter (fun k _ -> match k with
| (MemRef.ROffsetR _) -> false
| (MemRef.UOffsetR _) -> false
| _ -> true
) a

let clear_mr a (mr: MemRef.t) = MemRefTopMap.filter (fun k v -> (compare k mr <> 0)) a

let eval_varnode (a: t) (d: OctagonD.t) (vn: PCode.varNode) =
  match vn.varNode_node with
| PCode.Const c -> AbsVal.of_const c
| PCode.Unique u -> (match (MemRefTopMap.find_opt (MemRef.UniqueR u) a) with
  | None -> AbsVal.of_interval (OctagonD.request_interval d (MemRef.UniqueR u))
  | Some v -> (AbsVal.meet (AbsVal.of_interval (OctagonD.request_interval d (MemRef.UniqueR u))) v)
)
| PCode.Register r -> (match (MemRefTopMap.find_opt (MemRef.RegisterR r) a) with
  | None -> AbsVal.of_interval (OctagonD.request_interval d (MemRef.RegisterR r))
  | Some v -> (AbsVal.meet (AbsVal.of_interval (OctagonD.request_interval d (MemRef.RegisterR r))) v)
)
| PCode.Ram _ -> AbsVal.top

let process_load (p: PCode.prog) (a: t) (d: OctagonD.t) (pointerv: PCode.varNode) (outv: PCode.varNode) =
  match pointerv.varNode_node, MemRef.convert_varnode outv with
  | Unique u, Some outmr ->
    (match Option.bind (MemRefTopMap.find_opt (MemRef.UniqueR u) a) (fun x -> AbsVal.try_concretize x 20) with
    | Some vset -> MemRefTopMap.add outmr (AbsVal.of_limset (LimSetD.LimSet (Int64Set.map (fun x -> PCode.get_rom p x outv.varNode_width) vset))) a
    | None -> 
    MemRefTopMap.add outmr (AbsVal.of_interval (OctagonD.request_interval d (MemRef.UniqueR u))) a)
  | _, Some outmr -> clear_mr a outmr
  | _ -> a


let process_assignment (a: t) (d: OctagonD.t) (asn: PCode.assignable) (outv: PCode.varNode) =
  match MemRef.convert_varnode outv with
| None -> a
| Some outmr ->
  let na = clear_mr a outmr in
 (match asn with
| PCode.Avar vn -> MemRefTopMap.add outmr (eval_varnode a d vn) na
| PCode.Abop (PCode.Bint_add, op1v, op2v) ->
    let vn1 = eval_varnode a d op1v in
    let vn2 = eval_varnode a d op2v in
    MemRefTopMap.add outmr (AbsVal.add vn1 vn2 outv.varNode_width) na
| PCode.Abop (PCode.Bint_sub, op1v, op2v) ->
  let vn1 = eval_varnode a d op1v in
  let vn2 = eval_varnode a d op2v in
  MemRefTopMap.add outmr (AbsVal.sub vn1 vn2 outv.varNode_width) na
| PCode.Abop (PCode.Bint_mult, op1v, op2v) ->
  let vn1 = eval_varnode a d op1v in
  let vn2 = eval_varnode a d op2v in
  MemRefTopMap.add outmr (AbsVal.mul vn1 vn2 outv.varNode_width) na
| PCode.Abop (_, _, _) -> na
| PCode.Auop (PCode.Uint_sext, vn) -> let v = (eval_varnode a d vn) in
  MemRefTopMap.add outmr (AbsVal.sext v vn.varNode_width outv.varNode_width) na
| PCode.Auop (PCode.Uint_zext, vn) -> MemRefTopMap.add outmr (eval_varnode a d vn) na
| PCode.Auop (uop, opv) -> na
)