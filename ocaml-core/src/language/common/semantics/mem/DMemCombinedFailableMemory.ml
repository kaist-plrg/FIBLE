include Byte8Map

type t = { ram : Storable.t Byte8Map.t; rom : DMem.t }

let from_rom (rom : DMem.t) : t = { ram = Byte8Map.empty; rom }

let find (s : t) (addr : Byte8.t) : Storable.t =
  match Byte8Map.find_opt addr s.ram with
  | Some v -> v
  | None -> Byte (DMem.get_byte s.rom (addr |> Byte8.get_offset))

let load_mem (s : t) (addr : Byte8.t) (width : Int32.t) :
    (NumericValue.t, String.t) Result.t =
  let rec aux (addr : Byte8.t) (width : Int32.t) (acc : Storable.t list) :
      (Storable.t list, String.t) Result.t =
    if width = 0l then Ok acc
    else
      let c = find s addr in
      aux (Byte8.succ addr) (Int32.pred width) (c :: acc)
  in
  let* chars = aux addr width [] in
  Ok (List.rev chars)

let load_string (s : t) (addr : Byte8.t) : (string, String.t) Result.t =
  let rec aux (addr : Byte8.t) (acc : string) : (string, String.t) Result.t =
    let* c =
      find s addr |> function
      | Byte c -> Ok c
      | Undef -> Error "Undefined memory"
    in
    if c = Char.chr 0 then Ok acc
    else aux (Byte8.succ addr) (acc ^ String.make 1 c)
  in
  aux addr ""

let load_bytes (s : t) (addr : Byte8.t) (length : Int32.t) :
    (String.t, String.t) Result.t =
  let rec aux (addr : Byte8.t) (length : Int32.t) (acc : string) :
      (String.t, String.t) Result.t =
    if length = 0l then Ok acc
    else
      let* c =
        find s addr |> function
        | Byte c -> Ok c
        | Undef -> Error "Undefined memory"
      in
      aux (Byte8.succ addr) (Int32.pred length) (acc ^ String.make 1 c)
  in
  aux addr length ""

let store_mem (s : t) (addr : Byte8.t) (v : NumericValue.t) : t =
  let chars = v in
  let rec aux (addr : Byte8.t) (chars : Storable.t list)
      (acc : Storable.t Byte8Map.t) : Storable.t Byte8Map.t =
    match chars with
    | [] -> acc
    | c :: chars ->
        let acc = Byte8Map.add addr c acc in
        aux (Byte8.succ addr) chars acc
  in
  { s with ram = aux addr chars s.ram }

let store_bytes (s : t) (addr : Byte8.t) (bytes : string) : t =
  let rec aux (addr : Byte8.t) (bytes : string) (acc : Storable.t Byte8Map.t) :
      Storable.t Byte8Map.t =
    match bytes with
    | "" -> acc
    | bytes ->
        let c = String.get bytes 0 in
        let acc = Byte8Map.add addr (Storable.Byte c) acc in
        aux (Byte8.succ addr) (String.sub bytes 1 (String.length bytes - 1)) acc
  in
  { s with ram = aux addr bytes s.ram }

let undef_mem (s : t) (addr : Byte8.t) (length : Int32.t) : t =
  let rec aux (addr : Byte8.t) (length : Int32.t) (acc : Storable.t Byte8Map.t)
      : Storable.t Byte8Map.t =
    if length = 0l then acc
    else
      let acc = Byte8Map.add addr Storable.Undef acc in
      aux (Byte8.succ addr) (Int32.pred length) acc
  in
  { s with ram = aux addr length s.ram }
