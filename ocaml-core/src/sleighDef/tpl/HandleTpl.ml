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
  | _ -> [%logstr "Expected 7 children"] |> Result.error

let getFixedHandle (v : t) (opers : FixedHandle.t List.t)
    (pinfo : PatternInfo.t) : (FixedHandle.t, String.t) Result.t =
  match v.ptrspace with
  | Real _ -> (
      (* TODO: maybe different behavior *)
      let* space = ConstTpl.fixSpace v.space opers in
      let* size =
        ConstTpl.fix v.size opers pinfo |> Result.map Int64.to_int32
      in
      match v.ptroffset with
      | Handle v ->
          let* handle =
            List.nth_opt opers (v.handleInd |> Int32.to_int)
            |> Option.to_result ~none:[%logstr "getFixedHandle real Handle"]
          in
          {
            FixedHandle.space;
            size;
            offset_space = handle.offset_space;
            offset_offset = handle.offset_offset;
            offset_size = handle.offset_size;
            temp_offset = handle.temp_offset;
            temp_space = handle.temp_space;
          }
          |> Result.ok
      | _ ->
          (* TODO: wrapOffset *)
          let* offset_offset = ConstTpl.fix v.ptroffset opers pinfo in
          {
            FixedHandle.space;
            size;
            offset_space = None;
            offset_offset;
            offset_size = 0l;
            temp_offset = 0L;
            temp_space = None;
          }
          |> Result.ok)
  | _ ->
      let* space = ConstTpl.fixSpace v.space opers in
      let* size =
        ConstTpl.fix v.size opers pinfo |> Result.map Int64.to_int32
      in
      let* offset_offset = ConstTpl.fix v.ptroffset opers pinfo in
      let* offset_space = ConstTpl.fixSpace v.ptrspace opers in
      if AddrSpace.is_const_space offset_space then
        {
          FixedHandle.space;
          size;
          offset_space = None;
          offset_offset;
          offset_size = 0l;
          temp_offset = 0L;
          temp_space = None;
        }
        |> Result.ok
      else
        let* offset_size =
          ConstTpl.fix v.ptrsize opers pinfo |> Result.map Int64.to_int32
        in
        let* temp_space = ConstTpl.fixSpace v.temp_space opers in
        let* temp_offset = ConstTpl.fix v.temp_offset opers pinfo in
        {
          FixedHandle.space;
          size;
          offset_space = Some offset_space;
          offset_offset;
          offset_size;
          temp_offset;
          temp_space = Some temp_space;
        }
        |> Result.ok
