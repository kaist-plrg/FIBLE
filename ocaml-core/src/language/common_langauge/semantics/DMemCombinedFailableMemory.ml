open Basic
open Basic_collection
include AddrMap

let ( let* ) = Result.bind

type storable = Byte of Char.t | Undef
type t = { ram : storable AddrMap.t; rom : DMem.t }

let from_rom (rom : DMem.t) : t = { ram = AddrMap.empty; rom }

let find (s : t) (addr : Addr.t) : storable =
  match AddrMap.find_opt addr s.ram with
  | Some v -> v
  | None -> Byte (DMem.get_byte s.rom addr)

let load_mem (s : t) (addr : Addr.t) (width : Int32.t) :
    (NumericValue.t, String.t) Result.t =
  let rec aux (addr : Addr.t) (width : Int32.t) (acc : Char.t list) :
      (Char.t list, String.t) Result.t =
    if width = 0l then Ok acc
    else
      let c = find s addr in
      match c with
      | Undef -> Error "Undefined memory"
      | Byte c -> aux (Addr.succ addr) (Int32.pred width) (c :: acc)
  in
  let* chars = aux addr width [] in
  Ok (NumericValue.of_chars (List.rev chars))

let load_string (s : t) (addr : Addr.t) : (string, String.t) Result.t =
  let rec aux (addr : Addr.t) (acc : string) : (string, String.t) Result.t =
    let* c =
      find s addr |> function
      | Byte c -> Ok c
      | Undef -> Error "Undefined memory"
    in
    if c = Char.chr 0 then Ok acc
    else aux (Addr.succ addr) (acc ^ String.make 1 c)
  in
  aux addr ""

let load_bytes (s : t) (addr : Addr.t) (length : Int32.t) :
    (String.t, String.t) Result.t =
  let rec aux (addr : Addr.t) (length : Int32.t) (acc : string) :
      (String.t, String.t) Result.t =
    if length = 0l then Ok acc
    else
      let* c =
        find s addr |> function
        | Byte c -> Ok c
        | Undef -> Error "Undefined memory"
      in
      aux (Addr.succ addr) (Int32.pred length) (acc ^ String.make 1 c)
  in
  aux addr length ""

let store_mem (s : t) (addr : Addr.t) (v : NumericValue.t) : t =
  let chars = NumericValue.to_chars v in
  let rec aux (addr : Addr.t) (chars : Char.t list) (acc : storable AddrMap.t) :
      storable AddrMap.t =
    match chars with
    | [] -> acc
    | c :: chars ->
        let acc = AddrMap.add addr (Byte c) acc in
        aux (Addr.succ addr) chars acc
  in
  { s with ram = aux addr chars s.ram }

let store_bytes (s : t) (addr : Addr.t) (bytes : string) : t =
  let rec aux (addr : Addr.t) (bytes : string) (acc : storable AddrMap.t) :
      storable AddrMap.t =
    match bytes with
    | "" -> acc
    | bytes ->
        let c = String.get bytes 0 in
        let acc = AddrMap.add addr (Byte c) acc in
        aux (Addr.succ addr) (String.sub bytes 1 (String.length bytes - 1)) acc
  in
  { s with ram = aux addr bytes s.ram }

let undef_mem (s : t) (addr : Addr.t) (length : Int32.t) : t =
  let rec aux (addr : Addr.t) (length : Int32.t) (acc : storable AddrMap.t) :
      storable AddrMap.t =
    if length = 0l then acc
    else
      let acc = AddrMap.add addr Undef acc in
      aux (Addr.succ addr) (Int32.pred length) acc
  in
  { s with ram = aux addr length s.ram }
