type t = { start : Int64.t; size : Int32.t; bytes : String.t }

let create start bytes =
  { start; size = Int32.of_int (String.length bytes); bytes }

let get_byte (m : t) (addr : Int64.t) : Char.t Option.t =
  if Int64.compare addr m.start < 0 then None
  else if Int64.compare addr (Int64.add m.start (Int64.of_int32 m.size)) >= 0
  then None
  else
    let offset = Int64.to_int (Int64.sub addr m.start) in
    Some (String.get m.bytes offset)
