open DS;;

type t = (OctagonD.t * OctagonD.t) MemRefTopMap.t

let join = MemRefTopMap.mapjoin (fun (oct, ocf) (oct', ocf') -> 
 (OctagonD.join oct oct', OctagonD.join ocf ocf'))

let ole = MemRefTopMap.mapole (fun (oct, ocf) (oct', ocf') -> 
 OctagonD.ole oct oct' && OctagonD.ole ocf ocf') (OctagonD.top, OctagonD.top)

let top = MemRefTopMap.top

let pp = MemRefTopMap.pp (fun fmt (oct, ocf) -> 
 Format.fprintf fmt "(%a, %a)" OctagonD.pp oct OctagonD.pp ocf)

let clear_memref a = MemRefTopMap.filter (fun k v -> match k with
 | MemRef.RegisterR _ -> true
 | MemRef.UniqueR _ -> true
 | _ -> false) a

let clear_mr a (mr: MemRef.t) = MemRefTopMap.filter (fun k v -> compare k mr <> 0) a

let process_assignment (a: t) (d: OctagonD.t) (asn: PCode.assignable) (outv: PCode.varNode) =
  match MemRef.convert_varnode outv with
  | None -> a
  | Some outmr -> 
    let na = clear_mr a outmr in
    (match asn with
   | PCode.Avar vn -> (MemRef.convert_varnode vn |> Option.bind) (fun mr -> MemRefTopMap.find_opt mr na) |> Option.map (fun v -> MemRefTopMap.add outmr v na) |> Option.value ~default:na
  | PCode.Abop (PCode.Bint_less, op1v, op2v) ->
    (match (op1v.varNode_node, op2v.varNode_node) with
     | (PCode.Unique u, PCode.Const c) -> (MemRefTopMap.add outmr ((OctagonD.gen_single_lt (OctagonD.gen_single_ge OctagonD.top (MemRef.UniqueR u) 0L) (MemRef.UniqueR u) c), (OctagonD.gen_single_ge OctagonD.top (MemRef.UniqueR u) c))) na
     | _ -> na
    )
  | PCode.Abop (PCode.Bint_equal, op1v, op2v) -> 
    (match (op1v.varNode_node, op2v.varNode_node) with
     | (PCode.Unique u, PCode.Const c) -> (MemRefTopMap.add outmr ((OctagonD.gen_single_eq OctagonD.top (MemRef.UniqueR u) 0L), OctagonD.top)) na
     | _ -> na
    )
  | PCode.Abop (PCode.Bint_sless, op1v, op2v) -> na
  | PCode.Abop (PCode.Bint_slessequal, op1v, op2v) -> na
  | PCode.Abop (PCode.Bbool_or, op1v, op2v) -> (match (op1v.varNode_node, op2v.varNode_node) with
    | (PCode.Register r1, PCode.Register r2) -> (match (MemRefTopMap.find_opt (MemRef.RegisterR r1) na, MemRefTopMap.find_opt (MemRef.RegisterR r2) na) with
     | (Some (dt, df), Some (dt2, df2)) -> MemRefTopMap.add outmr (OctagonD.join (OctagonD.refine_consts (OctagonD.meet dt d)) (OctagonD.refine_consts (OctagonD.meet dt2 d)), OctagonD.meet df df2) na
     | _ -> na)
    | _ -> na
    )
  | PCode.Abop (_, _, _) -> na
  | PCode.Auop (PCode.Ubool_negate, opv) -> (match opv.varNode_node with
    | PCode.Unique u -> (match (MemRefTopMap.find_opt (MemRef.UniqueR u) na) with
     | Some (d1, d2) -> MemRefTopMap.add outmr (d2, d1) na
     | _ -> na
    )
    | _ -> na
  )
  | PCode.Auop (_, _) -> na
  )