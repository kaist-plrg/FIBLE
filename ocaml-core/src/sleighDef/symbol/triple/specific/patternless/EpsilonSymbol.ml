open StdlibExt
open Notation

type t = TypeDef.epsilon_t

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) (header : SymbolHeader.t)
    : (t, String.t) Result.t =
  let* const_space = SleighInit.get_constant_space sleighInit in
  ({ name = header.name; id = header.id; scopeid = header.scopeid; const_space }
    : t)
  |> Result.ok

let get_name (symbol : t) : String.t = symbol.name
let get_id (symbol : t) : Int32.t = symbol.id
let get_scopeid (symbol : t) : Int32.t = symbol.scopeid
