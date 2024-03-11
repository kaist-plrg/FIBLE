open StdlibExt
open Basic
open Util
open Ctypes
open Common_language

type hidden_fn = Hide : ('a -> 'b) fn -> hidden_fn

let cgc_funcs : String.t List.t =
  [
    "_terminate";
    "transmit";
    "receive";
    "fdwait";
    "allocate";
    "deallocate";
    "cgc_random";
    "random";
  ]

let signature_map : (Interop.func_sig * hidden_fn) StringMap.t =
  StringMap.of_list
    [
      ( "_terminate",
        ( { Interop.params = [ Interop.T32 ]; result = T32 },
          Hide (int @-> returning int) ) );
      ( "transmit",
        ( {
            Interop.params =
              [
                Interop.T32;
                Interop.TIBuffer_dep 2;
                Interop.T64;
                Interop.TBuffer 8L;
              ];
            result = T32;
          },
          Hide
            (int @-> ocaml_string @-> int64_t @-> ocaml_bytes @-> returning int)
        ) );
      ( "receive",
        ( {
            Interop.params =
              [
                Interop.T32;
                Interop.TBuffer_dep 2;
                Interop.T64;
                Interop.TBuffer 8L;
              ];
            result = T32;
          },
          Hide
            (int @-> ocaml_bytes @-> int64_t @-> ocaml_bytes @-> returning int)
        ) );
      ( "fdwait",
        ( {
            Interop.params =
              [
                Interop.T32;
                Interop.TBuffer 128L;
                Interop.TBuffer 128L;
                Interop.TIBuffer 8L;
                Interop.TBuffer 4L;
              ];
            result = T32;
          },
          Hide
            (int @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_string
           @-> ocaml_bytes @-> returning int) ) );
      ( "allocate",
        ( {
            Interop.params = [ Interop.T64; Interop.T32; Interop.TBuffer 8L ];
            result = T32;
          },
          Hide (int64_t @-> int @-> ocaml_bytes @-> returning int) ) );
      ( "deallocate",
        ( { Interop.params = [ Interop.T64; Interop.T64 ]; result = T32 },
          Hide (int64_t @-> int64_t @-> returning int) ) );
      ( "cgc_random",
        ( {
            Interop.params =
              [ Interop.TBuffer_dep 1; Interop.T64; Interop.TBuffer 8L ];
            result = T32;
          },
          Hide (ocaml_bytes @-> int64_t @-> ocaml_bytes @-> returning int) ) );
      ( "random",
        ( {
            Interop.params =
              [ Interop.TBuffer_dep 1; Interop.T64; Interop.TBuffer 8L ];
            result = T32;
          },
          Hide (ocaml_bytes @-> int64_t @-> ocaml_bytes @-> returning int) ) );
    ]

let to_ctype : type t. t typ -> Interop.t -> t =
 fun type_sig arg ->
  match (type_sig, arg) with
  | Ctypes_static.Primitive Int8_t, Interop.V8 x -> Char.code x
  | Ctypes_static.Primitive Int16_t, Interop.V16 x -> Int32.to_int x
  | Ctypes_static.Primitive Int, Interop.V32 x -> Int32.to_int x
  | Ctypes_static.Primitive Int32_t, Interop.V32 x -> x
  | Ctypes_static.Primitive Int64_t, Interop.V64 x -> x
  | Ctypes_static.Primitive Uint8_t, Interop.V8 x ->
      Unsigned.UInt8.of_int (Char.code x)
  | Ctypes_static.Primitive Uint16_t, Interop.V16 x ->
      Unsigned.UInt16.of_int (Int32.to_int x)
  | Ctypes_static.Primitive Uint32_t, Interop.V32 x ->
      Unsigned.UInt32.of_int32 x
  | Ctypes_static.Primitive Uint64_t, Interop.V64 x ->
      Unsigned.UInt64.of_int64 x
  | ( Ctypes_static.View
        { ty = Ctypes_static.Pointer (Ctypes_static.Primitive Char) },
      Interop.VString x ) ->
      Obj.magic x
  | Ctypes_static.OCaml Ctypes_static.Bytes, Interop.VBuffer x ->
      Ctypes.ocaml_bytes_start x
  | Ctypes_static.OCaml Ctypes_static.String, Interop.VIBuffer x ->
      Ctypes.ocaml_string_start x
  | _ -> [%log fatal "Not implemented"]

let to_interop : type t. t typ -> t -> Interop.t =
 fun type_sig arg ->
  match type_sig with
  | Ctypes_static.Primitive Int8_t -> Interop.V8 (Char.chr arg)
  | Ctypes_static.Primitive Int16_t -> Interop.V16 (Int32.of_int arg)
  | Ctypes_static.Primitive Int -> Interop.V32 (Int32.of_int arg)
  | Ctypes_static.Primitive Int32_t -> Interop.V32 arg
  | Ctypes_static.Primitive Int64_t -> Interop.V64 arg
  | Ctypes_static.Primitive Uint8_t ->
      Interop.V8 (Char.chr (Unsigned.UInt8.to_int arg))
  | Ctypes_static.Primitive Uint16_t ->
      Interop.V16 (Int32.of_int (Unsigned.UInt16.to_int arg))
  | Ctypes_static.Primitive Uint32_t ->
      Interop.V32 (Unsigned.UInt32.to_int32 arg)
  | Ctypes_static.Primitive Uint64_t ->
      Interop.V64 (Unsigned.UInt64.to_int64 arg)
  | Ctypes_static.View
      { ty = Ctypes_static.Pointer (Ctypes_static.Primitive Char) } ->
      Interop.VString (Obj.magic arg)
  | _ -> [%log fatal "Not implemented"]

let rec call_with_signature : type a. a fn -> a -> Interop.t list -> Interop.t =
 fun func_sig f args ->
  match (func_sig, args) with
  | Ctypes_static.Function (a, Returns x), h :: [] ->
      to_interop x (f (to_ctype a h))
  | Ctypes_static.Function (a, b), h :: rest ->
      call_with_signature b (f (to_ctype a h)) rest
  | _ -> [%log fatal "Not implemented"]

type event_t =
  | EventTerminate
  | EventReturn of (Int.t * Interop.t) list * Interop.t

let request_call (fname : String.t) (arg : Interop.t list) : event_t =
  if List.mem fname cgc_funcs then (
    Global.initialize_cgc_lib ();
    match fname with
    | "_terminate" -> EventTerminate
    | "transmit" ->
        let _, Hide fn = StringMap.find_opt fname signature_map |> Option.get in
        EventReturn
          ( [ (3, List.nth arg 3) ],
            call_with_signature fn
              (Foreign.foreign ~from:(!Global.cgc_lib |> Option.get) fname fn)
              arg )
    | "receive" ->
        let _, Hide fn = StringMap.find_opt fname signature_map |> Option.get in
        EventReturn
          ( [ (1, List.nth arg 1); (3, List.nth arg 3) ],
            call_with_signature fn
              (Foreign.foreign ~from:(!Global.cgc_lib |> Option.get) fname fn)
              arg )
    | "fdwait" ->
        let _, Hide fn = StringMap.find_opt fname signature_map |> Option.get in
        EventReturn
          ( [ (1, List.nth arg 1); (2, List.nth arg 2); (4, List.nth arg 4) ],
            call_with_signature fn
              (Foreign.foreign ~from:(!Global.cgc_lib |> Option.get) fname fn)
              arg )
    | "allocate" -> (
        match (List.nth arg 0, List.nth arg 2) with
        | Interop.V64 v, Interop.VBuffer b ->
            Bytes.set_int64_le b 0 !Global.global_blk_offset;
            Global.global_blk_offset := Int64.add v !Global.global_blk_offset;
            EventReturn ([ (2, Interop.VBuffer b) ], Interop.V32 0l)
        | _ -> [%log fatal "Not reacahble"])
    | "deallocate" -> EventReturn ([], Interop.V32 0l)
    | "cgc_random" ->
        let _, Hide fn = StringMap.find_opt fname signature_map |> Option.get in
        EventReturn
          ( [ (0, List.nth arg 0); (2, List.nth arg 2) ],
            call_with_signature fn
              (Foreign.foreign ~from:(!Global.cgc_lib |> Option.get) fname fn)
              arg )
    | "random" ->
        let _, Hide fn = StringMap.find_opt fname signature_map |> Option.get in
        EventReturn
          ( [ (0, List.nth arg 0); (2, List.nth arg 2) ],
            call_with_signature fn
              (Foreign.foreign
                 ~from:(!Global.cgc_lib |> Option.get)
                 "cgc_random" fn)
              arg )
    | _ -> [%log fatal "Not reacahble"])
  else
    match StringMap.find_opt fname signature_map with
    | Some (_, Hide fn) ->
        EventReturn ([], call_with_signature fn (Foreign.foreign fname fn) arg)
    | None -> [%log fatal "Not implemented"]

let request_call_opt (fname : String.t) (arg : Interop.t list) :
    ((Int.t * Interop.t) list * Interop.t) Option.t =
  match request_call fname arg with
  | EventReturn (a, b) -> Some (a, b)
  | EventTerminate -> None
