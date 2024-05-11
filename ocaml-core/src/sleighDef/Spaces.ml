open StdlibExt
open Notation

type t = AddrSpace.t List.t

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  let child = XmlExt.children xml in
  let* spaces = List.map AddrSpace.decode child |> ResultExt.join_list in
  AddrSpace.const_space :: spaces |> Result.ok

let get_constant_space (v : t) : (AddrSpace.t, String.t) Result.t =
  AddrSpace.const_space |> Result.ok

let get_space_by_name (v : t) (name : String.t) :
    (AddrSpace.t, String.t) Result.t =
  List.find_opt (fun x -> AddrSpace.get_name x = name) v
  |> Option.to_result ~none:"AddrSpace not found"
