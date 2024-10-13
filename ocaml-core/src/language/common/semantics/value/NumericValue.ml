open Storable

type t = Storable.t List.t

let zero width = List.init (Int32.to_int width) (fun _ -> Byte '\x00')
let undefined width = List.init (Int32.to_int width) (fun _ -> Undef)

let fully_defined (v : t) =
  List.for_all
    (fun (x : Storable.t) -> match x with Byte _ -> true | Undef -> false)
    v

let isZero (x : t) : Bool.t Option.t =
  if List.for_all (fun s -> Storable.compare (Byte '\x00') s = 0) x then
    Some true
  else if List.exists (fun s -> Storable.compare (Byte '\x00') s <> 0) x then
    Some false
  else None

let try_isZero (x : t) : (Bool.t, String.t) Result.t =
  isZero x |> Option.to_result ~none:"NumericValue.try_isZero: unknown"

let width (x : t) = Int32.of_int (List.length x)

let extend (x : t) (size : Int32.t) =
  if List.length x >= Int32.to_int size then x
  else
    let pad =
      List.init (Int32.to_int size - List.length x) (fun _ -> Byte '\x00')
    in
    List.append x pad

let extend_undef (x : t) (size : Int32.t) =
  if List.length x >= Int32.to_int size then x
  else
    let pad = List.init (Int32.to_int size - List.length x) (fun _ -> Undef) in
    List.append x pad

let try_string (x : t) : (String.t, String.t) Result.t =
  List.fold_right
    (fun x acc ->
      let* c = Storable.try_byte x in
      Result.map (fun (acc : Char.t List.t) -> c :: acc) acc)
    x (Ok [])
  |> Result.map (fun x -> List.to_seq x |> String.of_seq)

let value_signed_z (x : t) : (Z.t, String.t) Result.t =
  let* s = try_string x in
  Z.signed_extract (Z.of_bits s) 0 (String.length s * 8) |> Result.ok

let value_z (x : t) : (Z.t, String.t) Result.t =
  let* s = try_string x in
  Z.of_bits s |> Result.ok

let value_float (x : t) : (Float.t, String.t) Result.t =
  let* s = try_string x in
  try Float.of_string s |> Result.ok with Invalid_argument s -> Result.error s

let value_64 (x : t) : (Int64.t, String.t) Result.t =
  let* s = try_string (extend x 8l) in
  String.get_int64_le s 0 |> Result.ok

let value_32 (x : t) : (Int32.t, String.t) Result.t =
  let* s = try_string (extend x 4l) in
  String.get_int32_le s 0 |> Result.ok

let try_addr (x : t) : (Byte8.t, String.t) Result.t =
  let* v = value_64 x in
  Byte8.of_int64 v |> Result.ok

let try_loc (x : t) : (Loc.t, String.t) Result.t =
  let* addr = try_addr x in
  Loc.of_addr addr |> Result.ok

let pp fmt (x : t) =
  Format.fprintf fmt "@[%a:%d@]"
    (Format.pp_print_list Storable.pp)
    (List.to_seq x |> List.of_seq |> List.rev)
    (List.length x)

let pp_float fmt (x : t) =
  match List.length x with
  | 4 -> (
      match value_32 x with
      | Ok x -> Format.fprintf fmt "%f:4" (Int32.float_of_bits x)
      | Error _ ->
          Format.fprintf fmt "float(%a):4" (Format.pp_print_list Storable.pp) x)
  | 8 -> (
      match value_64 x with
      | Ok x -> Format.fprintf fmt "%f:8" (Int64.float_of_bits x)
      | Error _ ->
          Format.fprintf fmt "float(%a):8" (Format.pp_print_list Storable.pp) x)
  | _ ->
      Format.fprintf fmt "float(%a):%d"
        (Format.pp_print_list Storable.pp)
        x (List.length x)

let of_int64_le (x : Int64.t) (width : Int32.t) : t =
  let rec loop acc i v =
    if i < 0 then acc
    else
      let c = Byte (Char.chr (Int64.to_int (Int64.logand v 0xffL))) in
      loop (c :: acc) (i - 1) (Int64.shift_right_logical v 8)
  in
  List.rev (loop [] (Int32.to_int width - 1) x)

let of_chars (cs : Char.t list) : t = List.map (fun c -> Byte c) cs

let try_chars (x : t) : (Char.t list, String.t) Result.t =
  List.fold_right
    (fun x acc ->
      let* c = Storable.try_byte x in
      Result.map (fun (acc : Char.t List.t) -> c :: acc) acc)
    x (Ok [])

let of_z v width =
  let bits = Z.extract v 0 (Int32.to_int width * 8) |> Z.to_bits in
  let bits_padded = bits ^ String.make (Int32.to_int width) '\x00' in
  let bits_truncated = String.sub bits_padded 0 (Int32.to_int width) in
  of_chars (String.to_seq bits_truncated |> List.of_seq)

let of_float (v : Float.t) (width : Int32.t) : t =
  match v with
  | F32 v ->
      if Int32.compare width 4l != 0 then
        [%log
          raise
            (Invalid_argument
               (Format.asprintf
                  "NumericValue.of_float: %a does not fit in %ld bytes"
                  Float32.pp v width))];
      of_chars (Float32.to_string v |> String.to_seq |> List.of_seq)
  | F64 v ->
      if Int32.compare width 8l != 0 then
        [%log
          raise
            (Invalid_argument
               (Format.asprintf
                  "NumericValue.of_float: %a does not fit in %ld bytes"
                  Float64.pp v width))];
      of_chars (Float64.to_string v |> String.to_seq |> List.of_seq)
  | F80 v ->
      if Int32.compare width 10l != 0 then
        [%log
          raise
            (Invalid_argument
               (Format.asprintf
                  "NumericValue.of_float: %a does not fit in %ld bytes"
                  Float80.pp v width))];
      of_chars (Float80.to_string v |> String.to_seq |> List.of_seq)

let of_int64 v width =
  let bits = Int64.bitwidth v in
  if Int64.to_int32 bits > Int32.mul width 8l then
    [%log
      raise
        (Invalid_argument
           (Format.sprintf
              "NumericValue.of_int64: %Ld does not fit in %ld bytes" v width))]
  else of_int64_le v width

let of_int64_safe (v : Int64.t) (width : Int32.t) : (t, String.t) Result.t =
  let bits = Int64.bitwidth v in
  if Int64.to_int32 bits > Int32.mul width 8l then
    Error
      (Format.sprintf
         "NumericValue.of_int64_safe: %Ld does not fit in %ld bytes" v width)
  else Ok (of_int64_le v width)

let rec sublist (x : t) (offset : Int.t) (size : Int.t) : t =
  match x with
  | [] ->
      if offset = 0 && size = 0 then []
      else raise (Invalid_argument "sublist_empty")
  | x :: rest ->
      if offset = 0 && size > 0 then x :: sublist rest offset (size - 1)
      else if offset = 0 && size = 0 then []
      else if offset < 0 then raise (Invalid_argument "sublist_invalid_offset")
      else sublist rest (offset - 1) size

let get (x : t) (offset : Int32.t) (size : Int32.t) : t =
  sublist x (Int32.to_int offset) (Int32.to_int size)

let set (orig : t) (inserted : t) (offset : Int32.t) =
  if List.length orig < Int32.to_int offset + List.length inserted then
    [%log
      error "NumericValue.set %a %a %ld: out of range" pp orig pp inserted
        offset]
  else
    let left = sublist orig 0 (Int32.to_int offset) in
    let right =
      sublist orig
        (Int32.to_int offset + List.length inserted)
        (List.length orig - Int32.to_int offset - List.length inserted)
    in
    List.append (List.append left inserted) right

let subsume (a : t) (b : t) : Bool.t =
  List.length a = List.length b && List.for_all2 Storable.subsume a b

let of_num (n : t) : t = n
let sp (_ : SPVal.t) : t = failwith "NumericValue.sp: not supported"
