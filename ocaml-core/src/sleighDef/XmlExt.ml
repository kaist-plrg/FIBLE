include Xml

let attrib (x : Xml.xml) (attr : String.t) : (String.t, String.t) Result.t =
  try Xml.attrib x attr |> Result.ok
  with Xml.No_attribute att ->
    Format.asprintf "tag %s no exist" att |> Result.error

let attrib_fmt (x : Xml.xml) (attr : String.t)
    (fstr :
      ( 'a,
        Scanf.Scanning.in_channel,
        'b,
        ('d -> 'e) -> 'e option,
        'a -> 'd option,
        'd )
      format6) (mapper : 'e -> 'd) : ('d, String.t) Result.t =
  let* r = attrib x attr in
  Scanf.sscanf_opt r fstr mapper
  |> Option.to_result ~none:(Format.sprintf "scan failed %s" r)

let attrib_fmt_value (x : Xml.xml) (attr : String.t)
    (fstr :
      ( 'a,
        Scanf.Scanning.in_channel,
        'b,
        ('d -> 'e) -> 'e option,
        'a -> 'd option,
        'd )
      format6) (mapper : 'e -> 'd) (default : 'd) : 'd =
  match attrib_fmt x attr fstr mapper with Ok s -> s | Error _ -> default

let attrib_intb (x : Xml.xml) (attr : String.t) : (Int64.t, String.t) Result.t =
  attrib_fmt x attr "%Li" Fun.id

let attrib_int (x : Xml.xml) (attr : String.t) : (Int32.t, String.t) Result.t =
  attrib_fmt x attr "%li" Fun.id

let attrib_int_value (x : Xml.xml) (attr : String.t) (default : Int32.t) :
    Int32.t =
  attrib_fmt_value x attr "%li" Fun.id default

let attrib_bool (x : Xml.xml) (attr : String.t) : (Bool.t, String.t) Result.t =
  attrib_fmt x attr "%B" Fun.id

let attrib_bool_value (x : Xml.xml) (attr : String.t) (default : Bool.t) :
    Bool.t =
  attrib_fmt_value x attr "%B" Fun.id default

let attrib_hex (x : Xml.xml) (attr : String.t) : (Int64.t, String.t) Result.t =
  attrib_fmt x attr "0x%Lx" Fun.id

let check_tag (x : Xml.xml) (tag : String.t) : (Unit.t, String.t) Result.t =
  if String.equal (Xml.tag x) tag then Ok ()
  else
    Error
      (Format.asprintf "tag %s is not same as assertion: %s" (Xml.tag x) tag)

let child_tag_fst (x : Xml.xml) (tag : String.t) : (Xml.xml, String.t) Result.t
    =
  let c = Xml.children x in
  List.find_opt (fun c -> String.equal (Xml.tag c) tag) c
  |> Option.to_result ~none:"not found tag"

let single_child (x : Xml.xml) : (Xml.xml, String.t) Result.t =
  match Xml.children x with [ x ] -> Ok x | _ -> Error "not single child"
