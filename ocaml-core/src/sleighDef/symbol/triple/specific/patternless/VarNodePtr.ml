type t = P of Int32.t

let get_id (P id) : Int32.t = id
let of_int32 id = P id
