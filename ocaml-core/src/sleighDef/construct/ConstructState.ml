type t = { ct : ConstructorPtr.t; offset : Int32.t }

let get_constructor (v : t) : ConstructorPtr.t = v.ct
let get_offset (v : t) : Int32.t = v.offset
