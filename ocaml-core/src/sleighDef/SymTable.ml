type ptr_t = {
  curScope : Scope.t;
  scopeMap : Scope.t Int32Map.t;
  symbolHeaderMap : SymbolHeader.t Int32Map.t;
  symbolMap : Symbol.ptr_t Int32Map.t;
}

type t = {
  curScope : Scope.t;
  scopeMap : Scope.t Int32Map.t;
  symbolMap : SubtableSymbol.t Int32Map.t;
}

let partition_list (l : 'a List.t) (x : Int.t) (y : Int.t) :
    'a List.t * 'a List.t * 'a List.t =
  let a, b, c, _, _ =
    List.fold_left
      (fun (a, b, c, x, y) e ->
        match (x, y) with
        | 0, 0 -> (a, b, e :: c, 0, 0)
        | 0, y -> (a, e :: b, c, 0, y - 1)
        | x, y -> (e :: a, b, c, x - 1, y))
      ([], [], [], x, y) l
  in
  (a, b, c)

let add_scopes (scopes : Xml.xml List.t) :
    (Scope.t Int32Map.t, String.t) Result.t =
  Result.fold_left_M
    (fun acc smap ->
      let* () = XmlExt.check_tag smap "scope"
      and* id = XmlExt.attrib_int smap "id"
      and* parent = XmlExt.attrib_int smap "parent" in
      let parscope = if Int32.equal parent id then None else Some parent in
      let scope = Scope.symbol_scope parscope id in
      Int32Map.add id scope acc |> Result.ok)
    Int32Map.empty scopes

let add_symbolhds (symbolhds : Xml.xml List.t) (scopeMap : Scope.t Int32Map.t) :
    (SymbolHeader.t Int32Map.t * Scope.t Int32Map.t, String.t) Result.t =
  Result.fold_left_M
    (fun (symbolMap, scopeMap) smap ->
      let* symbolhd = SymbolHeader.decode smap in
      let* scope =
        Int32Map.find_opt symbolhd.scopeid scopeMap
        |> Option.to_result ~none:"No scope"
      in
      ( Int32Map.add symbolhd.id symbolhd symbolMap,
        Int32Map.add symbolhd.scopeid
          (Scope.add_symbol scope symbolhd.id)
          scopeMap )
      |> Result.ok)
    (Int32Map.empty, scopeMap) symbolhds

let add_symbols (symbols : Xml.xml List.t)
    (symbolHeaderMap : SymbolHeader.t Int32Map.t) (sleighInit : SleighInit.t) :
    (Symbol.ptr_t Int32Map.t, String.t) Result.t =
  Result.fold_left_M
    (fun symbolMap smap ->
      let* symbol = Symbol.decode smap symbolHeaderMap sleighInit in
      Int32Map.add (Symbol.get_id symbol) symbol symbolMap |> Result.ok)
    Int32Map.empty symbols

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) :
    (ptr_t, String.t) Result.t =
  let* () = XmlExt.check_tag xml "symbol_table"
  and* scopesize = XmlExt.attrib_int xml "scopesize" |> Result.map Int32.to_int
  and* symbolsize =
    XmlExt.attrib_int xml "symbolsize" |> Result.map Int32.to_int
  in
  let child = XmlExt.children xml in
  let scopes, symbolhds, symbols = partition_list child scopesize symbolsize in
  let* scopeMap = add_scopes scopes in
  let* symbolHeaderMap, scopeMap = add_symbolhds symbolhds scopeMap in
  let* symbolMap = add_symbols symbols symbolHeaderMap sleighInit in
  let* curScope =
    Int32Map.find_opt 0l scopeMap |> Option.to_result ~none:"No scope 0"
  in
  { curScope; scopeMap; symbolHeaderMap; symbolMap } |> Result.ok

let get_root (s : ptr_t) : (SubtableSymbol.ptr_t, String.t) Result.t =
  let v =
    Int32Map.filter_map
      (fun _ (v : Symbol.ptr_t) ->
        match v with
        | Triple (Subtable s) when String.equal s.name "instruction" -> Some s
        | _ -> None)
      s.symbolMap
  in
  match Int32Map.min_binding_opt v with
  | Some (_, s) -> Ok s
  | None -> Error "No root symbol"

let get_symbol (s : ptr_t) (pt : SymbolPtr.t) :
    (Symbol.ptr_t, String.t) Result.t =
  Int32Map.find_opt (SymbolPtr.get_id pt) s.symbolMap
  |> Option.to_result ~none:"not found symbol id"

let get_subtable (s : t) (pt : SubtablePtr.t) :
    (SubtableSymbol.t, String.t) Result.t =
  Int32Map.find_opt (SubtablePtr.get_id pt) s.symbolMap
  |> Option.to_result ~none:"not found subtable id"

let get_reg_spec (s : ptr_t) : (RegSpec.t, String.t) Result.t =
  let regs =
    Int32Map.filter_map
      (fun _ (v : Symbol.ptr_t) ->
        match v with
        | Triple (Tuple (Specific (Patternless (VarNode x)))) ->
            if AddrSpace.get_name x.space = "register" then Some x else None
        | _ -> None)
      s.symbolMap
    |> Int32Map.bindings |> List.map snd
  in
  let baseRegMap =
    List.fold_left
      (fun acc (x : VarNodeSymbol.t) ->
        Seq.fold_left
          (fun (acc : VarNodeSymbol.t Int32Map.t) (o : Int32.t) ->
            match Int32Map.find_opt o acc with
            | Some reg ->
                if Int32.compare reg.size x.size < 0 then Int32Map.add o x acc
                else acc
            | None -> Int32Map.add o x acc)
          acc
          (Seq.init (x.size |> Int32.to_int) (fun i ->
               Int32.add x.offset (Int32.of_int i))))
      Int32Map.empty regs
  in
  let baseRegs =
    List.filter
      (fun (x : VarNodeSymbol.t) ->
        match Int32Map.find_opt x.offset baseRegMap with
        | Some r -> r.name = x.name
        | None -> false)
      regs
  in
  let base_size =
    List.map (fun (x : VarNodeSymbol.t) -> (x.offset, x.size)) baseRegs
    |> Int32Map.of_list
  in
  let all_regs =
    List.filter_map
      (fun (x : VarNodeSymbol.t) ->
        match Int32Map.find_opt x.offset baseRegMap with
        | Some r ->
            Some
              ( (x.offset, x.size),
                (x.name, r.offset, Int32.sub x.offset r.offset) )
        | _ -> None)
      regs
    |> RegSpec.TMap.of_list
  in

  { RegSpec.base_size; all_regs } |> Result.ok
