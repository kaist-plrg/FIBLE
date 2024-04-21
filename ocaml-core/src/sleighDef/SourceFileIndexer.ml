type t = Xml.xml

let decode (xml : Xml.xml) : (t, String.t) Result.t = xml |> Result.ok
