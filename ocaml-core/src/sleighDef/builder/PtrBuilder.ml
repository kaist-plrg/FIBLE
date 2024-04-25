open StdlibExt
open Notation

let get_operand_raw (P ptr : OperandPtr.t) (map : TypeDef.sym_ptr_t Int32Map.t)
    : (OperandSymbol.ptr_t, String.t) Result.t =
  let* t =
    Int32Map.find_opt ptr map
    |> Option.to_result ~none:"OperandPtr not found in map"
  in
  Symbol.try_operand t |> Option.to_result ~none:"OperandPtr not an operand"

let get_tuple_raw (p : TuplePtr.t) (map : TypeDef.sym_ptr_t Int32Map.t) :
    (TupleSymbol.ptr_t, String.t) Result.t =
  let id = TuplePtr.get_id p in
  let* t =
    Int32Map.find_opt id map
    |> Option.to_result ~none:"TriplePtr not found in map"
  in
  Symbol.try_tuple t |> Option.to_result ~none:"TuplePtr not an tuple"

let get_triple_raw (p : TriplePtr.t) (map : TypeDef.sym_ptr_t Int32Map.t) :
    (TripleSymbol.ptr_t, String.t) Result.t =
  let id = TriplePtr.get_id p in
  let* t =
    Int32Map.find_opt id map
    |> Option.to_result ~none:"TriplePtr not found in map"
  in
  Symbol.try_triple t |> Option.to_result ~none:"TriplePtr not an triple"

let get_varnode_raw (p : VarNodePtr.t) (map : TypeDef.sym_ptr_t Int32Map.t) :
    (VarNodeSymbol.t, String.t) Result.t =
  let id = VarNodePtr.get_id p in
  let* t =
    Int32Map.find_opt id map
    |> Option.to_result ~none:"VarNodePtr not found in map"
  in
  Symbol.try_varnode t |> Option.to_result ~none:"VarNodePtr not an varnode"

let build_varnodelist (v : VarNodeListSymbol.ptr_t)
    (map : TypeDef.sym_ptr_t Int32Map.t) :
    (VarNodeListSymbol.t, String.t) Result.t =
  let* varNodeIds =
    v.varNodeIds
    |> List.map (fun ptr ->
           match ptr with
           | Some ptr -> get_varnode_raw ptr map |> Result.map Option.some
           | None -> None |> Result.ok)
    |> ResultExt.join_list
  in
  Ok { v with varNodeIds }

let build_value (v : ValueSymbol.ptr_t) (map : TypeDef.sym_ptr_t Int32Map.t) :
    (ValueSymbol.t, String.t) Result.t =
  match v with
  | PureValue s -> TypeDef.PureValue s |> Result.ok
  | Context s -> TypeDef.Context s |> Result.ok
  | Name s -> TypeDef.Name s |> Result.ok
  | ValueMap s -> TypeDef.ValueMap s |> Result.ok
  | VarNodeList s ->
      build_varnodelist s map |> Result.map (fun s -> TypeDef.VarNodeList s)

let build_family (f : FamilySymbol.ptr_t) (map : TypeDef.sym_ptr_t Int32Map.t) :
    (FamilySymbol.t, String.t) Result.t =
  match f with
  | Value s -> build_value s map |> Result.map (fun s -> TypeDef.Value s)

let rec build_specific (s : SpecificSymbol.ptr_t)
    (map : TypeDef.sym_ptr_t Int32Map.t) (fuel : Int.t) :
    (SpecificSymbol.t, String.t) Result.t =
  if fuel <= 0 then Error "Out of fuel"
  else
    match s with
    | End s -> TypeDef.End s |> Result.ok
    | Start s -> TypeDef.Start s |> Result.ok
    | Next2 s -> TypeDef.Next2 s |> Result.ok
    | Patternless s -> TypeDef.Patternless s |> Result.ok
    | Operand s ->
        build_operand 0xfffl s map (fuel - 1)
        |> Result.map (fun s -> TypeDef.Operand s)

and build_operand (id : Int32.t) (op : OperandSymbol.ptr_t)
    (map : TypeDef.sym_ptr_t Int32Map.t) (fuel : Int.t) :
    (OperandSymbol.t, String.t) Result.t =
  if fuel <= 0 then Error "Out of fuel"
  else
    let* operand_value =
      match op.operand_value with
      | OTriple (Either.Left ptr) ->
          get_triple_raw (TriplePtr.Tuple ptr) map
          |> Fun.flip Result.bind (fun t ->
                 match (t : TripleSymbol.ptr_t) with
                 | Tuple t ->
                     let* t = build_tuple t map (fuel - 1) in
                     TypeDef.OTriple (Either.Left t) |> Result.ok
                 | Subtable s ->
                     TypeDef.OTriple (Either.Right (SubtablePtr.of_int32 s.id))
                     |> Result.ok)
      | OTriple (Either.Right ptr) -> "not reachable" |> Result.error
      | ODefExp f -> TypeDef.ODefExp f |> Result.ok
    in
    Ok { op with operand_value }

and build_tuple (t : TupleSymbol.ptr_t) (map : TypeDef.sym_ptr_t Int32Map.t)
    (fuel : Int.t) : (TupleSymbol.t, String.t) Result.t =
  if fuel <= 0 then Error "Out of fuel"
  else
    match t with
    | Specific s ->
        build_specific s map (fuel - 1)
        |> Result.map (fun s -> TypeDef.Specific s)
    | Family f -> build_family f map |> Result.map (fun f -> TypeDef.Family f)

and build_triple (t : TripleSymbol.ptr_t) (map : TypeDef.sym_ptr_t Int32Map.t)
    (fuel : Int.t) : (TripleSymbol.t, String.t) Result.t =
  if fuel <= 0 then Error "Out of fuel"
  else
    match t with
    | Tuple s ->
        build_tuple s map (fuel - 1) |> Result.map (fun s -> TypeDef.Tuple s)
    | Subtable f ->
        build_subtable f map (fuel - 1)
        |> Result.map (fun f -> TypeDef.Subtable f)

and build_constructor (s : Constructor.ptr_t)
    (map : TypeDef.sym_ptr_t Int32Map.t) (fuel : Int.t) :
    (Constructor.t, String.t) Result.t =
  if fuel <= 0 then Error "Out of fuel"
  else
    let* operandIds =
      s.operandIds
      |> List.map (fun ptr ->
             let* optr = get_operand_raw ptr map in
             build_operand (SubtablePtr.get_id s.parentId) optr map (fuel - 1))
      |> ResultExt.join_list
    in
    { s with operandIds } |> Result.ok

and build_constructor_map (s : ConstructorMap.ptr_t)
    (map : TypeDef.sym_ptr_t Int32Map.t) (fuel : Int.t) :
    (ConstructorMap.t, String.t) Result.t =
  if fuel <= 0 then Error "Out of fuel"
  else
    let* nmap =
      s.map |> Int32Map.to_list
      |> List.map (fun (k, v) ->
             let* v = build_constructor v map (fuel - 1) in
             (k, v) |> Result.ok)
      |> ResultExt.join_list
    in
    { s with map = Int32Map.of_list nmap } |> Result.ok

and build_subtable (s : SubtableSymbol.ptr_t)
    (map : TypeDef.sym_ptr_t Int32Map.t) (fuel : Int.t) :
    (SubtableSymbol.t, String.t) Result.t =
  if fuel <= 0 then Error "Out of fuel"
  else (
    [%log debug "Building subtable %ld %s" s.id s.name];
    let* (construct : ConstructorMap.t) =
      build_constructor_map s.construct map (fuel - 1)
    in
    let* decisiontree = SubtableSymbol.lift_middle construct s.decisiontree in
    { s with construct; decisiontree } |> Result.ok)

let build_subtable_full (s : SubtableSymbol.ptr_t)
    (map : TypeDef.sym_ptr_t Int32Map.t) : (SubtableSymbol.t, String.t) Result.t
    =
  build_subtable s map ((Int32Map.cardinal map + 1) * 7)
