open StdlibExt
open Notation

type t = {
  offset : Int32.t;
  nonzerosize : Int32.t;
  maskvalues : (Int32.t * Int32.t) List.t;
}

let decode (xml : Xml.xml) : (t, String.t) Result.t =
  let* () = XmlExt.check_tag xml "pat_block" in
  let* offset = XmlExt.attrib_int xml "offset" in
  let* nonzerosize = XmlExt.attrib_int xml "nonzero" in
  let* maskvalues =
    XmlExt.children xml
    |> List.map (fun xml ->
           let* () = XmlExt.check_tag xml "mask_word" in
           let* mask = XmlExt.attrib_int xml "mask" in
           let* value = XmlExt.attrib_int xml "val" in
           (mask, value) |> Result.ok)
    |> ResultExt.join_list
  in
  { offset; nonzerosize; maskvalues } |> Result.ok

let isContextMatch (v : t) (walker : ParserWalker.t) : Bool.t =
  (*
{
  if (nonzerosize<=0) return (nonzerosize==0);
  int4 off = offset;
  for(int4 i=0;i<maskvec.size();++i) {
    uintm data = walker.getContextBytes(off,sizeof(uintm));
    if ((maskvec[i] & data)!=valvec[i]) return false;
    off += sizeof(uintm);
  }
  return true;
}
*)
  let rec loop i off =
    if i = List.length v.maskvalues then true
    else
      let mask, value = List.nth v.maskvalues i in
      let data =
        ParserWalker.getContextBytes walker off 4l |> Result.to_option
      in
      match data with
      | None -> false
      | Some data ->
          if Int32.logand mask data <> value then false
          else loop (i + 1) (Int32.add off 4l)
  in
  if Int32.compare v.nonzerosize 0l <= 0 then v.nonzerosize = 0l
  else loop 0 v.offset

let isInstructionMatch (v : t) (walker : ParserWalker.t) : Bool.t =
  let rec loop i off =
    if i = List.length v.maskvalues then true
    else
      let mask, value = List.nth v.maskvalues i in
      let data =
        ParserWalker.getInstructionBytes walker off 4l |> Result.to_option
      in
      match data with
      | None -> false
      | Some data ->
          if Int32.logand mask data <> value then false
          else loop (i + 1) (Int32.add off 4l)
  in
  if Int32.compare v.nonzerosize 0l <= 0 then v.nonzerosize = 0l
  else loop 0 v.offset

let pp (fmt : Format.formatter) (v : t) : unit =
  Format.fprintf fmt
    "pat_block { offset = %ld; nonzero = %ld; maskvalues = %a }" v.offset
    v.nonzerosize
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt (mask, value) -> Format.fprintf fmt "(%lx, %lx)" mask value))
    v.maskvalues
