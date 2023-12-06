open StdlibExt
open Basic

type t = String.t

let zero width = String.make (Int32.to_int width) '\x00'
let isZero x = String.for_all (Char.equal '\x00') x
let width (x : t) = Int32.of_int (String.length x)
let value_64 (x : t) : Int64.t = String.get_int64_le x 0
let value_32 (x : t) : Int32.t = String.get_int32_le x 0
let to_addr (x : t) : Addr.t = value_64 x
let to_loc (x : t) : Loc.t = (to_addr x, 0)

let pp fmt (x : t) =
  Format.fprintf fmt "%a:%d"
    (Format.pp_print_list (fun fmt c -> Format.fprintf fmt "%02x" (Char.code c)))
    (String.to_seq x |> List.of_seq |> List.rev)
    (String.length x)

let pp_float fmt (x : t) =
  match String.length x with
  | 4 -> Format.fprintf fmt "%f:4" (Int32.float_of_bits (value_32 x))
  | 8 -> Format.fprintf fmt "%f:8" (Int64.float_of_bits (value_64 x))
  | _ ->
      Format.fprintf fmt "float(%a):%d"
        (Format.pp_print_list (fun fmt c ->
             Format.fprintf fmt "%02x" (Char.code c)))
        (String.to_seq x |> List.of_seq |> List.rev)
        (String.length x)

let of_int64_le (x : Int64.t) (width : Int32.t) : t =
  let rec loop acc i v =
    if i < 0 then acc
    else
      let c = Char.chr (Int64.to_int (Int64.logand v 0xffL)) in
      loop (c :: acc) (i - 1) (Int64.shift_right_logical v 8)
  in
  List.rev (loop [] (Int32.to_int width - 1) x) |> List.to_seq |> String.of_seq

let of_chars (cs : Char.t list) : t = List.to_seq cs |> String.of_seq
let to_chars (x : t) : Char.t list = String.to_seq x |> List.of_seq

let of_int64 v width =
  let bits = Int64Ext.bitwidth v in
  if Int64.to_int32 bits > Int32.mul width 8l then
    [%log
      raise
        (Invalid_argument
           (Format.sprintf
              "NumericValue.of_int64: %Ld does not fit in %ld bytes" v width))]
  else of_int64_le v width

let of_int64_safe (v : Int64.t) (width : Int32.t) : (t, String.t) Result.t =
  let bits = Int64Ext.bitwidth v in
  if Int64.to_int32 bits > Int32.mul width 8l then
    Error
      (Format.sprintf
         "NumericValue.of_int64_safe: %Ld does not fit in %ld bytes" v width)
  else Ok (of_int64_le v width)

let refine_width (x : t) (width : Int32.t) : t =
  if Int32.equal (String.length x |> Int32.of_int) width then x
  else
    let value = value_64 x in
    of_int64 (Int64Ext.cut_width value width) width

let replace_width (ov : t) (nv : t) (width : Int32.t) : t =
  let ovv = value_64 ov in
  let nvv = value_64 nv in
  let rv =
    if Int32.equal width 8l then Int64.zero
    else
      Int64.logand ovv
        (Int64.shift_left (-1L) (Int32.to_int (Int32.mul width 8l)))
  in
  of_int64
    (Int64.logor rv (Int64Ext.cut_width nvv width))
    (String.length ov |> Int32.of_int)

let get (x : t) (offset : Int32.t) (size : Int32.t) : t =
  String.sub x (Int32.to_int offset) (Int32.to_int size)

let extend (x : t) (size : Int32.t) =
  if String.length x >= Int32.to_int size then x
  else
    let pad = String.make (Int32.to_int size - String.length x) '\x00' in
    x ^ pad

let set (orig : t) (inserted : t) (offset : Int32.t) =
  if String.length orig < Int32.to_int offset + String.length inserted then
    [%log error "NumericValue.set: out of range"]
  else
    let left = String.sub orig 0 (Int32.to_int offset) in
    let right =
      String.sub orig
        (Int32.to_int offset + String.length inserted)
        (String.length orig - Int32.to_int offset - String.length inserted)
    in
    left ^ inserted ^ right
