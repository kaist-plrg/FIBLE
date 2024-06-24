type t = P of Int32.t * Int32.t

let make (offset : Int32.t) (table_id : Int32.t) : t = P (offset, table_id)
let get_table_id (P (offset, table_id)) = table_id
let get_offset (P (offset, table_id)) = offset
