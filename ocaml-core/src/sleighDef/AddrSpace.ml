open StdlibExt
open Notation

type t = Const | Unique | Other | General of String.t

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  match Xml.tag xml with
  | "space_unique" -> Ok Unique
  | "space_other" -> Ok Other
  | _ ->
      let* name = XmlExt.attrib xml "name" in
      General name |> Result.ok

let pp fmt = function
  | Const -> Format.pp_print_string fmt "const"
  | Unique -> Format.pp_print_string fmt "unique"
  | Other -> Format.pp_print_string fmt "other"
  | General s -> Format.pp_print_string fmt s

let const_space = Const
let unique_space = Unique
let is_const_space = function Const -> true | _ -> false
let is_unique_space = function Unique -> true | _ -> false

let get_name = function
  | Const -> "const"
  | Unique -> "unique"
  | Other -> "other"
  | General s -> s
