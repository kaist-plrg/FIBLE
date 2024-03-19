include AddrMap

let ( let* ) = Result.bind

type t = Storable.t AddrMap.t

let load_mem (s : t) (addr : Addr.t) (width : Int32.t) :
    (NumericValue.t, String.t) Result.t =
  let rec aux (addr : Addr.t) (width : Int32.t) (acc : Char.t list) :
      (Char.t list, String.t) Result.t =
    if width = 0l then Ok acc
    else
      let c =
        AddrMap.find_opt addr s
        |> Option.value ~default:(Storable.Byte (Char.chr 0))
      in
      match c with
      | Undef -> Error "Undefined memory"
      | Byte c -> aux (Addr.succ addr) (Int32.pred width) (c :: acc)
  in
  let* chars = aux addr width [] in
  Ok (NumericValue.of_chars (List.rev chars))

let load_string (s : t) (addr : Addr.t) : (string, String.t) Result.t =
  let rec aux (addr : Addr.t) (acc : string) : (string, String.t) Result.t =
    let* c =
      AddrMap.find_opt addr s |> function
      | Some (Byte c) -> Ok c
      | None -> Ok (Char.chr 0)
      | Some Undef -> Error "Undefined memory"
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
        AddrMap.find_opt addr s |> function
        | Some (Byte c) -> Ok c
        | None -> Ok (Char.chr 0)
        | Some Undef -> Error "Undefined memory"
      in
      aux (Addr.succ addr) (Int32.pred length) (acc ^ String.make 1 c)
  in
  aux addr length ""

let store_mem (s : t) (addr : Addr.t) (v : NumericValue.t) :
    Storable.t AddrMap.t =
  let chars = v in
  let rec aux (addr : Addr.t) (chars : Storable.t list)
      (acc : Storable.t AddrMap.t) : Storable.t AddrMap.t =
    match chars with
    | [] -> acc
    | c :: chars ->
        let acc = AddrMap.add addr c acc in
        aux (Addr.succ addr) chars acc
  in
  aux addr chars s

let store_bytes (s : t) (addr : Addr.t) (bytes : string) : Storable.t AddrMap.t
    =
  let rec aux (addr : Addr.t) (bytes : string) (acc : Storable.t AddrMap.t) :
      Storable.t AddrMap.t =
    match bytes with
    | "" -> acc
    | bytes ->
        let c = String.get bytes 0 in
        let acc = AddrMap.add addr (Storable.Byte c) acc in
        aux (Addr.succ addr) (String.sub bytes 1 (String.length bytes - 1)) acc
  in
  aux addr bytes s

let undef_mem (s : t) (addr : Addr.t) (length : Int32.t) : Storable.t AddrMap.t
    =
  let rec aux (addr : Addr.t) (length : Int32.t) (acc : Storable.t AddrMap.t) :
      Storable.t AddrMap.t =
    if length = 0l then acc
    else
      let acc = AddrMap.add addr Storable.Undef acc in
      aux (Addr.succ addr) (Int32.pred length) acc
  in
  aux addr length s
