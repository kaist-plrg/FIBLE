open StdlibExt
open Notation

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
    |> ResultExt.join_list
    |> Result.map Int32Map.of_list
  in

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
  and unique_allocatemask = XmlExt.attrib_int_value xml "numsections" 0l
  and numSections = XmlExt.attrib_int_value xml "uniqmask" 0l in
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

let rec resolve (s : t) (st : SubtableSymbol.t) (walker : ParserWalker.t) :
    (Constructor.mapped_t, String.t) Result.t =
  [%log info "Resolving subtable %s" st.name];
  let* v = DecisionNode.resolve st.decisiontree walker in
  List.iter (fun c -> [%log info "Context: %a" ContextChange.pp c]) v.context;
  let* op_resolved, _ =
    ResultExt.fold_left_M
      (fun (ops, offsetList) (op : OperandSymbol.t) ->
        let* ob =
          if op.offsetbase = -1l then
            ParserWalker.get_offset walker |> Result.ok
          else
            List.nth_opt (List.rev offsetList) (Int32.to_int op.offsetbase)
            |> Option.to_result ~none:"Offset not found"
        in
        let off = Int32.add ob op.reloffset in
        let nwalker = ParserWalker.replace_offset walker off in
        [%log info "Resolving operand at %d" (List.length ops)];
        let* nresolved = resolve_op s op nwalker in
        (nresolved :: ops, off :: offsetList) |> Result.ok)
      ([], []) v.operandIds
  in
  let op_resolved = List.rev op_resolved in
  TypeDef.C { v with operandIds = op_resolved } |> Result.ok

and resolve_op (v : t) (op : OperandSymbol.t) (walker : ParserWalker.t) :
    (OperandSymbol.mapped_t, String.t) Result.t =
  [%log info "Resolving operand %s" op.name];
  let opv = op.operand_value in
  let* (nop :
         ( Constructor.mapped_t TypeDef.tuple_t,
           Constructor.mapped_t )
         TypeDef.operand_elem) =
    match opv with
    | OTriple (Right x) ->
        let* subtable = deref_triple v x in
        [%log
          info "Resolving triple %s; walker offset: %ld" op.name
            (ParserWalker.get_offset walker)];
        let* c = resolve v subtable walker in
        TypeDef.OTriple (Right c) |> Result.ok
    | OTriple (Left x) -> (
        match x with
        | Family t -> TypeDef.OTriple (Left (TypeDef.Family t)) |> Result.ok)
    | ODefExp x -> TypeDef.ODefExp x |> Result.ok
  in
  let op = { op with operand_value = nop } in
  Result.ok op
