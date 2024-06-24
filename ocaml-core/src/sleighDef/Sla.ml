type ptr_t = {
  maxdelayslotbytes : Int32.t;
  unique_allocatemask : Int32.t;
  numSections : Int32.t;
  version : Int32.t;
  bigendian : Bool.t;
  align : Int32.t;
  uniqbase : Int64.t;
  sourcefiles : SourceFileIndexer.t;
  spaces : Spaces.t;
  symbol_table : SymTable.ptr_t;
  root : SubtablePtr.t;
}

type t = {
  maxdelayslotbytes : Int32.t;
  unique_allocatemask : Int32.t;
  numSections : Int32.t;
  version : Int32.t;
  bigendian : Bool.t;
  align : Int32.t;
  uniqbase : Int64.t;
  sourcefiles : SourceFileIndexer.t;
  spaces : Spaces.t;
  symbol_table : SymTable.t;
  spaceinfo : SpaceInfo.t;
  regspec : RegSpec.t;
  root : SubtableSymbol.t;
}

let build_from_sleighInit
    ({
       maxdelayslotbytes;
       unique_allocatemask;
       numSections;
       version;
       bigendian;
       align;
       uniqbase;
       sourcefiles;
       spaces;
     } :
      SleighInit.t) (symbol_table : SymTable.ptr_t) : (t, String.t) Result.t =
  let* root = SymTable.get_root symbol_table in
  let* root = PtrBuilder.build_subtable_full root symbol_table.symbolMap in
  let* symbolMap =
    Int32Map.filter_map
      (fun _ (s : Symbol.ptr_t) ->
        match s with Triple (Subtable s) -> Some s | _ -> None)
      symbol_table.symbolMap
    |> Int32Map.to_list
    |> List.map (fun (k, v) ->
           let* v' = PtrBuilder.build_subtable_full v symbol_table.symbolMap in
           (k, v') |> Result.ok)
    |> Result.join_list
    |> Result.map Int32Map.of_list
  in
  let* spaceinfo = Spaces.to_spaceinfo spaces in
  let* regspec = SymTable.get_reg_spec symbol_table in
  {
    maxdelayslotbytes;
    unique_allocatemask;
    numSections;
    version;
    bigendian;
    align;
    uniqbase;
    sourcefiles;
    spaces;
    symbol_table =
      {
        curScope = symbol_table.curScope;
        scopeMap = symbol_table.scopeMap;
        symbolMap;
      };
    spaceinfo;
    regspec;
    root;
  }
  |> Result.ok

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  let* () = XmlExt.check_tag xml "sleigh"
  and* version = XmlExt.attrib_int xml "version"
  and* bigendian = XmlExt.attrib_bool xml "bigendian"
  and* align = XmlExt.attrib_int xml "align"
  and* uniqbase = XmlExt.attrib_hex xml "uniqbase"
  and* sourcefiles =
    XmlExt.child_tag_fst xml "sourcefiles"
    |> Fun.flip Result.bind SourceFileIndexer.decode
  and* spaces =
    XmlExt.child_tag_fst xml "spaces" |> Fun.flip Result.bind Spaces.decode
  in
  let maxdelayslotbytes = XmlExt.attrib_int_value xml "maxdelay" 0l
  and unique_allocatemask = XmlExt.attrib_int_value xml "uniqmask" 0l
  and numSections = XmlExt.attrib_int_value xml "numsections" 0l in
  let sleighInit : SleighInit.t =
    {
      maxdelayslotbytes;
      unique_allocatemask;
      numSections;
      version;
      bigendian;
      align;
      uniqbase;
      sourcefiles;
      spaces;
    }
  in
  let* symbol_table =
    XmlExt.child_tag_fst xml "symbol_table"
    |> Fun.flip Result.bind (Fun.flip SymTable.decode sleighInit)
  in
  build_from_sleighInit sleighInit symbol_table

let deref_triple (v : t) (tp : SubtablePtr.t) :
    (SubtableSymbol.t, String.t) Result.t =
  SymTable.get_subtable v.symbol_table tp

let rec translate_ov (s : t) (ov : OperandValue.t) :
    (PatternExpression.t, String.t) Result.t =
  let* stable = SymTable.get_subtable s.symbol_table ov.table in
  let* constructor = SubtableSymbol.get_constructor stable ov.ctid in
  let* opsym =
    List.nth_opt constructor.operandIds (ov.index |> Int32.to_int)
    |> Option.to_result ~none:"opsym index out of bound"
  in
  match opsym.operand_value with
  | OTriple (Left a) -> (
      match a with
      | Specific (Operand a) -> translate_ov s a.localexp
      | Specific (End v) -> v.patexp |> Result.ok
      | Specific (Start v) -> v.patexp |> Result.ok
      | Specific (Next2 v) -> v.patexp |> Result.ok
      | Specific (Patternless v) ->
          PatternExpression.V
            (PatternValue.Constant (ConstantValue.of_int64 0L))
          |> Result.ok
      | Family a -> FamilySymbol.get_pattern a |> Result.ok)
  | OTriple (Right _) -> "no op in op" |> Result.error
  | ODefExp o -> translate_oe s o

and translate_oe (s : t) (oe : OperandExpression.t) :
    (PatternExpression.t, String.t) Result.t =
  match oe with
  | V (Pat v) -> PatternExpression.V v |> Result.ok
  | V (Oper v) -> translate_ov s v
  | Binary (bop, e1, e2) ->
      let* e1 = translate_oe s e1 in
      let* e2 = translate_oe s e2 in
      PatternExpression.Binary (bop, e1, e2) |> Result.ok
  | Unary (uop, e) ->
      let* e = translate_oe s e in
      PatternExpression.Unary (uop, e) |> Result.ok

let rec resolve (s : t) (st : SubtableSymbol.t) (walker : ParserWalker.t) :
    (Constructor.disas_t, String.t) Result.t =
  [%log debug "Resolving subtable %s" st.name];
  let* v = DecisionNode.resolve st.decisiontree walker in
  List.iter (fun c -> [%log debug "Context: %a" ContextChange.pp c]) v.context;
  let* nwalker =
    Result.fold_left_M
      (fun walker (c : ContextChange.t) ->
        ContextChange.apply c (translate_oe s) walker
          {
            PatternInfo.addr = 0L;
            naddr = 0L;
            n2addr = None;
            umask = 0l;
            uoffset = 0L;
          })
      walker v.context
  in
  let* op_resolved =
    Result.fold_left_M
      (fun ops (op : OperandSymbol.t) ->
        let* ob =
          if op.offsetbase = -1l then
            ParserWalker.get_offset nwalker |> Result.ok
          else
            let* (resolved_op : OperandSymbol.disas_t) =
              ([%log debug "Offsetbase: %ld" op.offsetbase];
               List.nth_opt (List.rev ops) (Int32.to_int op.offsetbase))
              |> Option.to_result ~none:"Offset not found"
            in
            Int32.add resolved_op.mapped.length resolved_op.mapped.offset
            |> Result.ok
        in
        let off = Int32.add ob op.reloffset in
        let nwalker = ParserWalker.replace_offset nwalker off in
        [%log debug "Current constructor: %a" Constructor.pp_printpiece v];
        [%log debug "Resolving operand at %d" (List.length ops)];
        let* nresolved = resolve_op s op nwalker in
        nresolved :: ops |> Result.ok)
      [] v.operandIds
  in
  let op_resolved = List.rev op_resolved in
  TypeDef.C { v with operandIds = op_resolved } |> Result.ok

and resolve_op (v : t) (op : OperandSymbol.t) (walker : ParserWalker.t) :
    (OperandSymbol.disas_t, String.t) Result.t =
  [%log debug "Resolving operand %s" op.name];
  [%log debug "Current walker offset: %ld" (ParserWalker.get_offset walker)];
  let opv = op.operand_value in
  let* (nop, length) : ('a, 'b) TypeDef.operand_elem * Int32.t =
    match opv with
    | OTriple (Right x) ->
        let* subtable = deref_triple v x in
        [%log
          debug "Resolving triple %s; walker offset: %ld" op.name
            (ParserWalker.get_offset walker)];
        let* c = resolve v subtable walker in
        ( TypeDef.OTriple (Right c),
          Constructor.calc_length c (ParserWalker.get_offset walker) )
        |> Result.ok
    | OTriple (Left x) ->
        let* v, length =
          match x with
          | Specific (Operand a) ->
              let* a = resolve_op v a walker in
              (TypeDef.Specific (Operand a), a.mapped.length) |> Result.ok
          | Specific (End v) ->
              (TypeDef.Specific (End v), op.minimumlength) |> Result.ok
          | Specific (Start v) ->
              (TypeDef.Specific (Start v), op.minimumlength) |> Result.ok
          | Specific (Next2 v) ->
              (TypeDef.Specific (Next2 v), op.minimumlength) |> Result.ok
          | Specific (Patternless v) ->
              (TypeDef.Specific (Patternless v), op.minimumlength) |> Result.ok
          | Family t -> (TypeDef.Family t, op.minimumlength) |> Result.ok
        in
        (TypeDef.OTriple (Left v), length) |> Result.ok
    | ODefExp x -> (TypeDef.ODefExp x, op.minimumlength) |> Result.ok
  in
  let op = { op with operand_value = nop } in
  Result.ok
    {
      op with
      mapped = { TypeDef.offset = ParserWalker.get_offset walker; length };
    }

let rec resolve_handle (s : t) (C st : Constructor.disas_t)
    (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (Constructor.handle_t * FixedHandle.t, String.t) Result.t =
  let* op_resolved =
    Result.fold_left_M
      (fun ops (op : OperandSymbol.disas_t) ->
        (* let* ob =
             if op.offsetbase = -1l then
               ParserWalker.get_offset walker |> Result.ok
             else
               let* (resolved_op : OperandSymbol.handle_t) =
                 ([%log debug "Offsetbase: %ld" op.offsetbase];
                  List.nth_opt (List.rev ops) (Int32.to_int op.offsetbase))
                 |> Option.to_result ~none:"Offset not found"
               in
               Int32.add resolved_op.mapped.length resolved_op.mapped.offset
               |> Result.ok
           in
           let off = Int32.add ob op.reloffset in
           let nwalker = ParserWalker.replace_offset walker off in *)
        [%log debug "Current constructor: %a" Constructor.pp_printpiece st];
        [%log debug "Resolving operand at %d" (List.length ops)];
        let* nresolved = resolve_handle_op s op walker pinfo in
        nresolved :: ops |> Result.ok)
      [] st.operandIds
  in
  let op_resolved = List.rev op_resolved in
  let* handle_const =
    match st.tmpl with
    | Some { resultTpl = Some v } ->
        HandleTpl.getFixedHandle v
          (op_resolved
          |> List.map (fun (x : OperandSymbol.handle_t) -> x.mapped.handle))
          pinfo
    | _ -> FixedHandle.of_constant 0L |> Result.ok
  in
  (* TODO: HandleTpl to FixedHandle *)
  (TypeDef.C { st with operandIds = op_resolved }, handle_const) |> Result.ok

and resolve_handle_op (v : t) (op : OperandSymbol.disas_t)
    (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (OperandSymbol.handle_t, String.t) Result.t =
  [%log debug "Resolving operand %s" op.name];
  [%log debug "Current walker offset: %ld" (ParserWalker.get_offset walker)];
  let nwalker = ParserWalker.replace_offset walker op.mapped.offset in
  let opv = op.operand_value in
  let* (nop, handle) : ('a, 'b) TypeDef.operand_elem * FixedHandle.t =
    match opv with
    | OTriple (Right c) ->
        let* c, handle = resolve_handle v c nwalker pinfo in
        (TypeDef.OTriple (Right c), handle) |> Result.ok
    | OTriple (Left x) ->
        let* v, handle =
          match x with
          | Specific (Operand a) ->
              let* a = resolve_handle_op v a nwalker pinfo in
              (TypeDef.Specific (Operand a), a.mapped.handle) |> Result.ok
          | Specific (End v) ->
              let* handle = EndSymbol.getFixedHandle v nwalker pinfo in
              (TypeDef.Specific (End v), handle) |> Result.ok
          | Specific (Start v) ->
              let* handle = StartSymbol.getFixedHandle v nwalker pinfo in
              (TypeDef.Specific (Start v), handle) |> Result.ok
          | Specific (Next2 v) ->
              let* handle = Next2Symbol.getFixedHandle v nwalker pinfo in
              (TypeDef.Specific (Next2 v), handle) |> Result.ok
          | Specific (Patternless v) ->
              let* handle = PatternlessSymbol.getFixedHandle v nwalker in
              (TypeDef.Specific (Patternless v), handle) |> Result.ok
          | Family t ->
              let* handle = FamilySymbol.getFixedHandle t nwalker pinfo in
              (TypeDef.Family t, handle) |> Result.ok
        in
        (TypeDef.OTriple (Left v), handle) |> Result.ok
    | ODefExp x ->
        let* pe = translate_oe v x in
        let* v = PatternExpression.get_value pe nwalker pinfo in
        let fv = FixedHandle.of_constant v in
        (TypeDef.ODefExp x, fv) |> Result.ok
  in
  let mop = { op with operand_value = nop } in
  Result.ok
    {
      mop with
      mapped =
        { TypeDef.offset = op.mapped.offset; handle; length = op.mapped.length };
    }
