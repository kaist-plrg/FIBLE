open StdlibExt
open Basic
open Util
open Ctypes
open Common_language

type hidden_fn = Hide : ('a -> 'b) fn -> hidden_fn

let signature_map : (Interop.func_sig * hidden_fn) StringMap.t =
  StringMap.of_list
    [
      ( "puts",
        ( { Interop.params = [ Interop.TString ]; result = T32 },
          Hide (string @-> returning int) ) );
      ( "printf",
        ( { Interop.params = [ Interop.TString ]; result = T32 },
          Hide (string @-> returning int) ) );
      (* Incorrect; todo *)
      ( "__printf_chk",
        ( { Interop.params = [ Interop.T32; Interop.TString ]; result = T32 },
          Hide (int @-> string @-> returning int) ) );
      (* Incorrect; todo *)
      ( "memcmp",
        ( {
            Interop.params = [ Interop.TString; Interop.TString; Interop.T32 ];
            result = T32;
          },
          Hide (string @-> string @-> int @-> returning int) ) );
      (* Incorrect; todo *)
      ( "fopen",
        ( { Interop.params = [ Interop.TString; Interop.TString ]; result = T64 },
          Hide (string @-> string @-> returning int64_t) ) );
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
  | _ -> failwith "Not implemented"

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
  | _ -> failwith "Not implemented"

let rec call_with_signature : type a. a fn -> a -> Interop.t list -> Interop.t =
 fun func_sig f args ->
  match (func_sig, args) with
  | Ctypes_static.Function (a, Returns x), h :: [] ->
      to_interop x (f (to_ctype a h))
  | Ctypes_static.Function (a, b), h :: rest ->
      call_with_signature b (f (to_ctype a h)) rest
  | _ -> failwith "Not implemented"

let request_call (fname : String.t) (arg : Interop.t list) : Interop.t =
  match StringMap.find_opt fname signature_map with
  | Some (_, Hide fn) -> call_with_signature fn (Foreign.foreign fname fn) arg
  | None -> failwith "Not implemented"
