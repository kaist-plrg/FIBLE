type t = TypeDef.next2_t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* const_space = SleighInit.get_constant_space sleighInit in
  let patexp = PatternExpression.of_next2 () in
  ({
     name = header.name;
     id = header.id;
     scopeid = header.scopeid;
     const_space;
     patexp;
   }
    : t)
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid

let print (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (String.t, String.t) Result.t =
  let* v =
    pinfo.n2addr
    |> Option.map Common.Byte8.get_offset
    |> Option.to_result ~none:"No n2addr"
  in
  Format.sprintf "0x%Lx" v |> Result.ok

let getFixedHandle (v : t) (walker : ParserWalker.t) (pinfo : PatternInfo.t) :
    (FixedHandle.t, String.t) Result.t =
  let* v =
    pinfo.n2addr
    |> Option.map Common.Byte8.get_offset
    |> Option.to_result ~none:"No n2addr"
  in
  FixedHandle.of_constant v |> Result.ok
