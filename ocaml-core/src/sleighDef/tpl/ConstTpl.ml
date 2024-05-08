open StdlibExt
open Notation

module HandleType = struct
  type t = Space | Offset | Size | OffsetPlus of Int32.t
end

type t =
  | Real of Int64.t
  | Handle of { handleInd : Int32.t; field : HandleType.t }
  | Start
  | Next
  | Next2
  | Curspace
  | CurspaceSize
  | Space of AddrSpace.t
  | Realtive of Int64.t
  | FlowRef
  | FlowRefSize
  | FlowDest
  | FlowDestSize

(* DIFFERENT FROM LATEST BUILD *)
let decode (xml : Xml.xml) (sleighInit : SleighInit.t) : (t, String.t) Result.t
    =
  (* for latest build
     let tag = XmlExt.tag xml in
     match tag with
     | "const_real" ->
         let* value = XmlExt.attrib_int xml "val" in
         Real value |> Result.ok
     | "const_handle" ->
         let* handleInd = XmlExt.attrib_int xml "val" in
         let* field =
           let* selector = XmlExt.attrib_int xml "s" in
           match selector with
           | 0l -> HandleType.Space |> Result.ok
           | 1l -> HandleType.Offset |> Result.ok
           | 2l -> HandleType.Size |> Result.ok
           | 3l ->
               let* offset = XmlExt.attrib_int xml "plus" in
               HandleType.OffsetPlus offset |> Result.ok
           | _ -> "Unknown handle selector" |> Result.error
         in
         Handle { handleInd; field } |> Result.ok
     | "const_start" -> Start |> Result.ok
     | "const_next" -> Next |> Result.ok
     | "const_next2" -> Next2 |> Result.ok
     | "curspace" -> Curspace |> Result.ok
     | "curspace_size" -> CurspaceSize |> Result.ok
     | "const_spaceid" ->
         let* spaceName = XmlExt.attrib xml "space" in
         let* space = SleighInit.get_space_by_name sleighInit spaceName in
         Space space |> Result.ok
     | "const_relative" ->
         let* offset = XmlExt.attrib_int xml "val" in
         Realtive offset |> Result.ok
     | "const_flowref" -> FlowRef |> Result.ok
     | "const_flowref_size" -> FlowRefSize |> Result.ok
     | "const_flowdest" -> FlowDest |> Result.ok
     | "const_flowdest_size" -> FlowDestSize |> Result.ok
     | _ -> Format.asprintf "Unknown handle type %s" tag |> Result.error
  *)
  let* tp = XmlExt.attrib xml "type" in
  match tp with
  | "real" ->
      let* value = XmlExt.attrib_intb xml "val" in
      Real value |> Result.ok
  | "handle" ->
      let* handleInd = XmlExt.attrib_int xml "val" in
      let* field =
        let* selector = XmlExt.attrib xml "s" in
        match selector with
        | "space" -> HandleType.Space |> Result.ok
        | "offset" -> HandleType.Offset |> Result.ok
        | "size" -> HandleType.Size |> Result.ok
        | "offset_plus" ->
            let* offset = XmlExt.attrib_int xml "plus" in
            HandleType.OffsetPlus offset |> Result.ok
        | _ -> "Unknown handle selector" |> Result.error
      in
      Handle { handleInd; field } |> Result.ok
  | "start" -> Start |> Result.ok
  | "next" -> Next |> Result.ok
  | "next2" -> Next2 |> Result.ok
  | "curspace" -> Curspace |> Result.ok
  | "curspace_size" -> CurspaceSize |> Result.ok
  | "spaceid" ->
      let* spaceName = XmlExt.attrib xml "name" in
      let* space = SleighInit.get_space_by_name sleighInit spaceName in
      Space space |> Result.ok
  | "relative" ->
      let* offset = XmlExt.attrib_intb xml "val" in
      Realtive offset |> Result.ok
  | "flowref" -> FlowRef |> Result.ok
  | "flowref_size" -> FlowRefSize |> Result.ok
  | "flowdest" -> FlowDest |> Result.ok
  | "flowdest_size" -> FlowDestSize |> Result.ok
  | _ -> Format.asprintf "Unknown handle type %s" tp |> Result.error

let try_real (h : t) : Int64.t option =
  match h with Real value -> Some value | _ -> None
