type tag =
  | T64
  | T32
  | T16
  | T8
  | TString
  | TFloat
  | TDouble
  | TList of tag
  | TBuffer of Int64.t
  | TBuffer_dep of Int.t
  | TIBuffer of Int64.t
  | TIBuffer_dep of Int.t
  | TVoid

type func_sig = { params : tag list; result : tag }

type t =
  | V64 of Int64.t
  | V32 of Int32.t
  | V16 of Int32.t
  | V8 of Char.t
  | VString of String.t
  | VFloat of Float.t
  | VDouble of Float.t
  | VList of tag * t list
  | VBuffer of Bytes.t
  | VIBuffer of String.t
  | VUnit

let rec pp fmt (v : t) =
  match v with
  | V64 i -> Format.fprintf fmt "%Ld" i
  | V32 i -> Format.fprintf fmt "%ld" i
  | V16 i -> Format.fprintf fmt "%ld" i
  | V8 i -> Format.fprintf fmt "%c" i
  | VString s -> Format.fprintf fmt "%s" s
  | VFloat f -> Format.fprintf fmt "%f" f
  | VDouble f -> Format.fprintf fmt "%f" f
  | VList (t, l) ->
      Format.fprintf fmt "[";
      List.iter (fun v -> Format.fprintf fmt "%a; " pp v) l;
      Format.fprintf fmt "]"
  | VBuffer a ->
      Format.fprintf fmt "[";
      Bytes.iter (fun c -> Format.fprintf fmt "%c" c) a;
      Format.fprintf fmt "]"
  | VIBuffer a ->
      Format.fprintf fmt "[";
      String.iter (fun c -> Format.fprintf fmt "%c" c) a;
      Format.fprintf fmt "]"
  | VUnit -> Format.fprintf fmt "()"

let extract_64 (v : t) =
  match v with
  | V64 i -> i
  | V32 i -> Int64.of_int32 i
  | V16 i -> Int64.of_int32 i
  | _ -> [%log error "extract_64"]
