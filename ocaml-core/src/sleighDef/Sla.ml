open StdlibExt
open Notation

type 'subtable_t poly_t = {
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
  root : 'subtable_t;
}

type ptr_t = SubtableSymbol.ptr_t poly_t
type t = SubtableSymbol.t poly_t

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
      SleighInit.t) (symbol_table : SymTable.t) : (t, String.t) Result.t =
  let* root = SymTable.get_root symbol_table in
  let* root = PtrBuilder.build_subtable root symbol_table.symbolMap in
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
    symbol_table;
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

let deref_triple (v : ptr_t) (tp : TriplePtr.t) :
    (TripleSymbol.ptr_t, String.t) Result.t =
  let* s = SymTable.get_symbol v.symbol_table (SymbolPtr.of_triple tp) in
  match s with
  | Triple s -> s |> Result.ok
  | _ -> "not triple symbol" |> Result.error
