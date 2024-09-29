open Util
open Ctypes
open Common

type hidden_fn = Hide : ('a -> 'b) fn -> hidden_fn

let env : String.t List.t = Unix.environment () |> Array.to_list
let global_syscall_offset = ref 0L

let x64_syscall_table (n : Int64.t) : Interop.func_sig Option.t =
  match n with
  | 0L (* read *) ->
      {
        Interop.params =
          ( [ ("x", T64) ],
            [ Interop.t64; Interop.mutable_charbuffer_of "x"; Interop.id "x" ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 2L (* open *) ->
      {
        Interop.params =
          ([], [ Interop.const_string_ptr; Interop.t64; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 3L (* close *) ->
      { Interop.params = ([], [ Interop.t64 ]); result = Some Interop.t64 }
      |> Option.some
  | 4L (* stat *) ->
      {
        Interop.params =
          ( [],
            [ Interop.const_string_ptr; Interop.mutable_charbuffer_fixed 144L ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 5L (* fstat *) ->
      {
        Interop.params =
          ([], [ Interop.t64; Interop.mutable_charbuffer_fixed 144L ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 6L (* lstat *) ->
      {
        Interop.params =
          ( [],
            [ Interop.const_string_ptr; Interop.mutable_charbuffer_fixed 144L ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 9L (* mmap *) ->
      {
        Interop.params =
          ( [],
            [
              Interop.t64;
              Interop.t64;
              Interop.t64;
              Interop.t64;
              Interop.t64;
              Interop.t64;
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 11L (* munmap *) ->
      {
        Interop.params = ([], [ Interop.t64; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 12L (* brk *) ->
      { Interop.params = ([], [ Interop.t64 ]); result = Some Interop.t64 }
      |> Option.some
  | 16L (* ioctl *) ->
      {
        Interop.params =
          ( [ ("x", T64) ],
            [
              Interop.t64;
              Interop.id "x";
              Interop.TMatch
                ( "x",
                  [
                    (0x5401L (* TCGETS *), Interop.mutable_charbuffer_fixed 36L);
                    ( 0x5402L (* TCSETS *),
                      Interop.immutable_charbuffer_fixed 36L );
                    ( 0x5403L (* TCSETSW *),
                      Interop.immutable_charbuffer_fixed 36L );
                    ( 0x5413L (* TIOCGWINSZ *),
                      Interop.mutable_charbuffer_fixed 8L );
                  ],
                  Interop.tany );
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 20L (* writev *) ->
      {
        Interop.params =
          ( [ ("x", T64) ],
            [
              Interop.t64;
              Interop.TPtr
                (Dynamic
                   (Interop.TIArr
                      ( Interop.Abs
                          ( ("y", T64),
                            TStruct
                              [
                                TPrim (Interop.immutable_charbuffer_of "y");
                                TPrim (TArith (TInt (Id "y")));
                              ] ),
                        Interop.Dependent "x" )));
              Interop.id "x";
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 60L (* exit *) ->
      { Interop.params = ([], [ Interop.t64 ]); result = Some Interop.t64 }
      |> Option.some
  | 72L (* fcntl *) ->
      {
        Interop.params = ([], [ Interop.t64; Interop.t64; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 79L (* getcwd *) ->
      {
        Interop.params =
          ([ ("x", T64) ], [ Interop.mutable_charbuffer_of "x"; Interop.id "x" ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 89L (* readlink *) ->
      {
        Interop.params =
          ( [ ("x", T64) ],
            [
              Interop.const_string_ptr;
              Interop.mutable_charbuffer_of "x";
              Interop.id "x";
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 161L (* chroot *) ->
      {
        Interop.params = ([], [ Interop.const_string_ptr ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 217L (* getdents64 *) ->
      {
        Interop.params =
          ( [ ("x", T64) ],
            [ Interop.t64; Interop.mutable_charbuffer_of "x"; Interop.id "x" ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 221L (* fadvise64 *) ->
      {
        Interop.params =
          ([], [ Interop.t64; Interop.t64; Interop.t64; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 231L (* fgetxattr; ignore *) ->
      { Interop.params = ([], [ Interop.t64 ]); result = Some Interop.t64 }
      |> Option.some
  | 257L (* openat *) ->
      {
        Interop.params =
          ( [],
            [ Interop.t64; Interop.const_string_ptr; Interop.t64; Interop.t64 ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 262L (* newfstatat *) ->
      {
        Interop.params =
          ( [],
            [
              Interop.t64;
              Interop.const_string_ptr;
              Interop.mutable_charbuffer_fixed 144L;
              Interop.t64;
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | _ -> Option.none

let x64_do_syscall (args : Interop.t list) : (Interop.t, String.t) Result.t =
  global_syscall_offset := Int64.add !global_syscall_offset 1L;
  let* rax, args =
    match args with
    | Interop.VArith (VInt (V64 rax)) :: rest -> Ok (rax, rest)
    | _ -> Error "syscall: invalid syscall number"
  in
  match (rax, args) with
  | 0L, [ VArith (VInt (V64 rdi)); VBuffer rsi; VArith (VInt (V64 rdx)) ] ->
      let retv = Util.read (rdi |> Int64.to_int) rsi rdx in
      [%log
        finfo "syscall" "READ ARG: %Ld %a %Ld" rdi Interop.pp (VBuffer rsi) rdx];
      [%log finfo "syscall" "READ RET: %Ld" retv];
      Interop.v64 retv |> Result.ok
  | 2L, [ VIBuffer sname; VArith (VInt (V64 flags)); VArith (VInt (V64 mode)) ]
    ->
      let name = Interop.vibuffer_to_string sname in
      let retv =
        Util.open_ name (flags |> Int64.to_int) (mode |> Int64.to_int)
      in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 3L, [ VArith (VInt (V64 rdi)) ] ->
      Interop.v64 (Util.close (rdi |> Int64.to_int) |> Int64.of_int)
      |> Result.ok
  | 4L, [ VIBuffer rsi; VBuffer rdx ] ->
      let* path = Interop.vibuffer_to_string rsi |> Result.ok in
      let retv = Util.stat path rdx in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 5L, [ VArith (VInt (V64 rdi)); VBuffer rsi ] ->
      let retv = Util.fstat (rdi |> Int64.to_int) rsi in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 6L, [ VIBuffer rsi; VBuffer rdx ] ->
      let* path = Interop.vibuffer_to_string rsi |> Result.ok in
      let retv = Util.lstat path rdx in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 9L, [ VArith (VInt (V64 rdi)); rsi; rdx; rcx; VArith (VInt (V64 r8)); r9 ]
    ->
      if r8 = 0xffffffffffffffffL then
        if rdi = 0L then
          let hval =
            (String.hash (Format.asprintf "%Lx" !global_syscall_offset)
            |> Int64.of_int |> Int64.shift_left)
              24
          in
          Interop.v64 hval |> Result.ok
        else Interop.v64 rdi |> Result.ok
      else Error "Not supported mmap"
  | 11L, [ rdi; rsi ] -> Interop.v64 0L |> Result.ok
  | 12L, [ VArith (VInt (V64 rdi)) ] -> Interop.v64 rdi |> Result.ok
  | 16L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 0x5401L)); VBuffer rdx ]
    ->
      (* TCGETS *)
      let retv = Util.tcgets (rdi |> Int64.to_int) rdx in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 16L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 0x5402L)); VIBuffer rdx ]
    ->
      (* TCSETS *)
      let* str = Interop.vibuffer_to_string rdx |> Result.ok in
      let retv = Util.tcsets (rdi |> Int64.to_int) str in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 16L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 0x5403L)); VIBuffer rdx ]
    ->
      (* TCSETSW *)
      let* str = Interop.vibuffer_to_string rdx |> Result.ok in
      let retv = Util.tcsetsw (rdi |> Int64.to_int) str in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 16L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 0x5413L)); VBuffer rdx ]
    ->
      (* TIOCGWINSZ *)
      let retv = Util.tiocgwinsz (rdi |> Int64.to_int) rdx in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 16L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 rsi)); VOpaque ] ->
      [%log error "not implemented ioctl for %Ld %Ld" rdi rsi]
  | 20L, [ VArith (VInt (V64 rdi)); VIBuffer rsi; VArith (VInt (V64 rdx)) ] ->
      [%log
        finfo "syscall" "WRITE ARG: %Ld %a %Ld" rdi Interop.pp (VIBuffer rsi)
          rdx];
      let* writearr =
        Array.map
          (fun x ->
            match x with
            | Interop.VStruct [ VIBuffer b; _ ] ->
                Interop.vibuffer_to_string b |> Result.ok
            | _ -> "parse fail" |> Result.error)
          rsi
        |> Array.to_list |> Result.join_list
      in
      let writestr = String.concat "" writearr in
      let retv =
        Util.write (Int64.to_int rdi) writestr
          (String.length writestr |> Int64.of_int)
      in
      Interop.v64 retv |> Result.ok
  | 60L, [ VArith (VInt (V64 rdi)) ] -> exit (Int64.to_int rdi)
  | ( 72L,
      [
        VArith (VInt (V64 rdi));
        VArith (VInt (V64 rsi));
        VArith (VInt (V64 rdx));
      ] )
  (* FCNTL *) -> (
      match rsi with
      | 0L (*DUPFD*) ->
          Ok
            (Interop.v64
               (Util.dupfd (rdi |> Int64.to_int) (rdx |> Int64.to_int)
               |> Int64.of_int))
      | 1L (*GETFD*) ->
          Ok (Interop.v64 (Util.getfd (rdi |> Int64.to_int) |> Int64.of_int))
      | 2L (*SETFD*) ->
          Ok
            (Interop.v64
               (Util.setfd (rdi |> Int64.to_int) (rdx |> Int64.to_int)
               |> Int64.of_int))
      | 3L (*GETFL*) ->
          Ok (Interop.v64 (Util.getfl (rdi |> Int64.to_int) |> Int64.of_int))
      | 4L (*SETFL*) ->
          Ok
            (Interop.v64
               (Util.setfl (rdi |> Int64.to_int) (rdx |> Int64.to_int)
               |> Int64.of_int))
      | 8L (*SETOWN*) ->
          Ok
            (Interop.v64
               (Util.setown (rdi |> Int64.to_int) (rdx |> Int64.to_int)
               |> Int64.of_int))
      | 9L (*GETOWN*) ->
          Ok (Interop.v64 (Util.getown (rdi |> Int64.to_int) |> Int64.of_int))
      | _ -> Error "unimplemented fcntl")
  | 79L, [ VBuffer rdi; VArith (VInt (V64 rsi)) ] ->
      let retv = Util.getcwd rdi (Int64.to_int rsi) in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 89L, [ VIBuffer rdi; VBuffer rsi; VArith (VInt (V64 rdx)) ] ->
      let* path = Interop.vibuffer_to_string rdi |> Result.ok in
      let retv = Util.readlink path rsi (rdx |> Int64.to_int) in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 161L, [ VIBuffer rsi ] ->
      let* path = Interop.vibuffer_to_string rsi |> Result.ok in
      let retv = Util.chroot path in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | 217L, [ VArith (VInt (V64 rdi)); VBuffer rsi; VArith (VInt (V64 rdx)) ] ->
      let retv =
        Util.getdents64 (rdi |> Int64.to_int) rsi (rdx |> Int64.to_int)
      in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | ( 221L (* fadvise64 *),
      [
        VArith (VInt (V64 rdi));
        VArith (VInt (V64 rsi));
        VArith (VInt (V64 rdx));
        VArith (VInt (V64 rcx));
      ] ) ->
      Interop.v64
        (Util.fadvise64 (rdi |> Int64.to_int) rsi rdx (rcx |> Int64.to_int)
        |> Int64.of_int)
      |> Result.ok
  | 231L, [ VArith (VInt (V64 rdi)) ] -> Interop.v64 0L |> Result.ok
  | ( 257L,
      [
        VArith (VInt (V64 rdi));
        VIBuffer rsi;
        VArith (VInt (V64 rdx));
        VArith (VInt (V64 rcx));
      ] ) ->
      let* path = Interop.vibuffer_to_string rsi |> Result.ok in
      let retv =
        Util.openat (rdi |> Int64.to_int) path (rdx |> Int64.to_int)
          (rcx |> Int64.to_int)
      in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | ( 262L,
      [
        VArith (VInt (V64 rdi));
        VIBuffer rsi;
        VBuffer rdx;
        VArith (VInt (V64 rcx));
      ] ) ->
      let* path = Interop.vibuffer_to_string rsi |> Result.ok in
      let retv =
        Util.newfstatat (rdi |> Int64.to_int) path rdx (rcx |> Int64.to_int)
      in
      Interop.v64 (Int64.of_int retv) |> Result.ok
  | _ -> Error (Format.sprintf "unimplemented syscall %Ld" rax)

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
        ( { Interop.params = ([], [ Interop.t32 ]); result = Some Interop.t32 },
          Hide (int @-> returning int) ) );
      ( "transmit",
        ( {
            Interop.params =
              ( [ ("x", T64) ],
                [
                  Interop.t32;
                  Interop.immutable_charbuffer_of "x";
                  Interop.id "x";
                  Interop.mutable_charbuffer_fixed 8L;
                ] );
            result = Some Interop.t32;
          },
          Hide
            (int @-> ocaml_string @-> int64_t @-> ocaml_bytes @-> returning int)
        ) );
      ( "receive",
        ( {
            Interop.params =
              ( [ ("x", T64) ],
                [
                  Interop.t32;
                  Interop.mutable_charbuffer_of "x";
                  Interop.id "x";
                  Interop.mutable_charbuffer_fixed 8L;
                ] );
            result = Some Interop.t32;
          },
          Hide
            (int @-> ocaml_bytes @-> int64_t @-> ocaml_bytes @-> returning int)
        ) );
      ( "fdwait",
        ( {
            Interop.params =
              ( [],
                [
                  Interop.t32;
                  Interop.mutable_charbuffer_fixed 128L;
                  Interop.mutable_charbuffer_fixed 128L;
                  Interop.immutable_charbuffer_fixed 8L;
                  Interop.mutable_charbuffer_fixed 4L;
                ] );
            result = Some Interop.t32;
          },
          Hide
            (int @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_string
           @-> ocaml_bytes @-> returning int) ) );
      ( "allocate",
        ( {
            Interop.params =
              ( [],
                [
                  Interop.t64; Interop.t32; Interop.mutable_charbuffer_fixed 8L;
                ] );
            result = Some Interop.t32;
          },
          Hide (int64_t @-> int @-> ocaml_bytes @-> returning int) ) );
      ( "deallocate",
        ( {
            Interop.params = ([], [ Interop.t64; Interop.t64 ]);
            result = Some Interop.t32;
          },
          Hide (int64_t @-> int64_t @-> returning int) ) );
      ( "cgc_random",
        ( {
            Interop.params =
              ( [ ("x", T64) ],
                [
                  Interop.mutable_charbuffer_of "x";
                  Interop.id "x";
                  Interop.mutable_charbuffer_fixed 8L;
                ] );
            result = Some Interop.t32;
          },
          Hide (ocaml_bytes @-> int64_t @-> ocaml_bytes @-> returning int) ) );
      ( "random",
        ( {
            Interop.params =
              ( [ ("x", T64) ],
                [
                  Interop.mutable_charbuffer_of "x";
                  Interop.id "x";
                  Interop.mutable_charbuffer_fixed 8L;
                ] );
            result = Some Interop.t32;
          },
          Hide (ocaml_bytes @-> int64_t @-> ocaml_bytes @-> returning int) ) );
    ]

let to_ctype : type t. t typ -> Interop.t -> t =
 fun type_sig arg ->
  match (type_sig, arg) with
  | Ctypes_static.Primitive Int8_t, Interop.VArith (VInt (V8 x)) -> Char.code x
  | Ctypes_static.Primitive Int16_t, Interop.VArith (VInt (V16 x)) ->
      Int32.to_int x
  | Ctypes_static.Primitive Int, Interop.VArith (VInt (V32 x)) -> Int32.to_int x
  | Ctypes_static.Primitive Int32_t, Interop.VArith (VInt (V32 x)) -> x
  | Ctypes_static.Primitive Int64_t, Interop.VArith (VInt (V64 x)) -> x
  | Ctypes_static.Primitive Uint8_t, Interop.VArith (VInt (V8 x)) ->
      Unsigned.UInt8.of_int (Char.code x)
  | Ctypes_static.Primitive Uint16_t, Interop.VArith (VInt (V16 x)) ->
      Unsigned.UInt16.of_int (Int32.to_int x)
  | Ctypes_static.Primitive Uint32_t, Interop.VArith (VInt (V32 x)) ->
      Unsigned.UInt32.of_int32 x
  | Ctypes_static.Primitive Uint64_t, Interop.VArith (VInt (V64 x)) ->
      Unsigned.UInt64.of_int64 x
  | ( Ctypes_static.View
        { ty = Ctypes_static.Pointer (Ctypes_static.Primitive Char) },
      Interop.VBuffer x ) ->
      Obj.magic (Bytes.to_string x)
  | Ctypes_static.OCaml Ctypes_static.Bytes, Interop.VBuffer x ->
      Ctypes.ocaml_bytes_start x
  | Ctypes_static.OCaml Ctypes_static.String, Interop.VIBuffer x ->
      Ctypes.ocaml_string_start (Interop.vibuffer_to_string x)
  | _ -> [%log fatal "Not implemented"]

let to_interop : type t. t typ -> t -> Interop.t =
 fun type_sig arg ->
  match type_sig with
  | Ctypes_static.Primitive Int8_t -> Interop.v8 (Char.chr arg)
  | Ctypes_static.Primitive Int16_t -> Interop.v16 (Int32.of_int arg)
  | Ctypes_static.Primitive Int -> Interop.v32 (Int32.of_int arg)
  | Ctypes_static.Primitive Int32_t -> Interop.v32 arg
  | Ctypes_static.Primitive Int64_t -> Interop.v64 arg
  | Ctypes_static.Primitive Uint8_t ->
      Interop.v8 (Char.chr (Unsigned.UInt8.to_int arg))
  | Ctypes_static.Primitive Uint16_t ->
      Interop.v16 (Int32.of_int (Unsigned.UInt16.to_int arg))
  | Ctypes_static.Primitive Uint32_t ->
      Interop.v32 (Unsigned.UInt32.to_int32 arg)
  | Ctypes_static.Primitive Uint64_t ->
      Interop.v64 (Unsigned.UInt64.to_int64 arg)
  | Ctypes_static.View
      { ty = Ctypes_static.Pointer (Ctypes_static.Primitive Char) } ->
      Interop.vstring (Obj.magic arg : String.t)
  | _ -> [%log fatal "Not implemented"]

let rec call_with_signature : type a. a fn -> a -> Interop.t list -> Interop.t =
 fun func_sig f args ->
  match (func_sig, args) with
  | Ctypes_static.Function (a, Returns x), h :: [] ->
      to_interop x (f (to_ctype a h))
  | Ctypes_static.Function (a, b), h :: rest ->
      call_with_signature b (f (to_ctype a h)) rest
  | _ -> [%log fatal "Not implemented"]

type event_t = EventTerminate | EventReturn of Interop.t

let request_call (fname : String.t) (arg : Interop.t list) : event_t =
  if List.mem fname cgc_funcs then (
    Global.initialize_cgc_lib ();
    match fname with
    | "_terminate" -> EventTerminate
    | "transmit" ->
        let _, Hide fn = StringMap.find_opt fname signature_map |> Option.get in
        EventReturn
          (call_with_signature fn
             (Foreign.foreign ~from:(!Global.cgc_lib |> Option.get) fname fn)
             arg)
    | "receive" ->
        let _, Hide fn = StringMap.find_opt fname signature_map |> Option.get in
        EventReturn
          (call_with_signature fn
             (Foreign.foreign ~from:(!Global.cgc_lib |> Option.get) fname fn)
             arg)
    | "fdwait" ->
        let _, Hide fn = StringMap.find_opt fname signature_map |> Option.get in
        EventReturn
          (call_with_signature fn
             (Foreign.foreign ~from:(!Global.cgc_lib |> Option.get) fname fn)
             arg)
    | "allocate" -> (
        match (List.nth arg 0, List.nth arg 2) with
        | Interop.VArith (VInt (V64 v)), Interop.VBuffer b ->
            let aligned_v =
              Int64.mul 4096L (Int64.div (Int64.add v 4095L) 4096L)
            in
            Bytes.set_int64_le b 0 !Global.global_blk_offset;
            Global.global_blk_offset :=
              Int64.add aligned_v !Global.global_blk_offset;
            EventReturn (Interop.v32 0l)
        | _ -> [%log fatal "Not reacahble"])
    | "deallocate" -> EventReturn (Interop.v32 0l)
    | "cgc_random" ->
        let _, Hide fn = StringMap.find_opt fname signature_map |> Option.get in
        EventReturn
          (call_with_signature fn
             (Foreign.foreign ~from:(!Global.cgc_lib |> Option.get) fname fn)
             arg)
    | "random" ->
        let _, Hide fn = StringMap.find_opt fname signature_map |> Option.get in
        EventReturn
          (call_with_signature fn
             (Foreign.foreign
                ~from:(!Global.cgc_lib |> Option.get)
                "cgc_random" fn)
             arg)
    | _ -> [%log fatal "Not reacahble"])
  else
    match StringMap.find_opt fname signature_map with
    | Some (_, Hide fn) ->
        EventReturn (call_with_signature fn (Foreign.foreign fname fn) arg)
    | None -> [%log fatal "Not implemented"]

let request_call_opt (fname : String.t) (arg : Interop.t list) :
    Interop.t Option.t =
  match request_call fname arg with
  | EventReturn b -> Some b
  | EventTerminate -> None
