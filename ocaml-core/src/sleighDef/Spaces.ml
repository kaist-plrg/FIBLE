type t = Xml.xml

let decode (xml : Xml.xml) : (t, String.t) Result.t = xml |> Result.ok

let get_constant_space (xml : t) : (AddrSpace.t, String.t) Result.t =
  () |> Result.ok

let get_space_by_name (xml : t) (name : String.t) :
    (AddrSpace.t, String.t) Result.t =
  () |> Result.ok
