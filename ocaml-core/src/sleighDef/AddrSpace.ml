open StdlibExt
open Notation

type t =
  | Const
  | Unique of Int.t
  | Other of Int.t
  | General of (Int.t * String.t)

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  let* index = XmlExt.attrib_int xml "index" in
  match Xml.tag xml with
  | "space_unique" -> Unique (index |> Int32.to_int) |> Result.ok
  | "space_other" -> Other (index |> Int32.to_int) |> Result.ok
  | _ ->
      let* name = XmlExt.attrib xml "name" in
      General (index |> Int32.to_int, name) |> Result.ok

let pp fmt = function
  | Const -> Format.pp_print_string fmt "const"
  | Unique _ -> Format.pp_print_string fmt "unique"
  | Other _ -> Format.pp_print_string fmt "other"
  | General (_, s) -> Format.pp_print_string fmt s

let const_space = Const
let unique_space (l : Int.t) = Unique l
let is_const_space = function Const -> true | _ -> false
let is_unique_space = function Unique _ -> true | _ -> false

let get_name = function
  | Const -> "const"
  | Unique _ -> "unique"
  | Other _ -> "other"
  | General (_, s) -> s

let get_index = function
  | Const -> 0
  | Unique l -> l
  | Other l -> l
  | General (l, _) -> l
