open StdlibExt
open Notation

type t = {
  space : ConstTpl.t;
  size : ConstTpl.t;
  ptrspace : ConstTpl.t;
  ptroffset : ConstTpl.t;
  ptrsize : ConstTpl.t;
  temp_space : ConstTpl.t;
  temp_offset : ConstTpl.t;
}

let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  let child = Xml.children xml in
  match child with
  | [ space; size; ptrspace; ptroffset; ptrsize; temp_space; temp_offset ] ->
      let* space = ConstTpl.decode space sleighInit in
      let* size = ConstTpl.decode size sleighInit in
      let* ptrspace = ConstTpl.decode ptrspace sleighInit in
      let* ptroffset = ConstTpl.decode ptroffset sleighInit in
      let* ptrsize = ConstTpl.decode ptrsize sleighInit in
      let* temp_space = ConstTpl.decode temp_space sleighInit in
      let* temp_offset = ConstTpl.decode temp_offset sleighInit in
      { space; size; ptrspace; ptroffset; ptrsize; temp_space; temp_offset }
      |> Result.ok
  | _ -> "Expected 7 children" |> Result.error
