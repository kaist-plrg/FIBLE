type t = { value : int64; width : int32 }

let pp fmt { value; width } = Format.fprintf fmt "%Ld:%ld" value width
