open Util
open Ctypes
open Common

type hidden_fn = Hide : ('a -> 'b) fn -> hidden_fn

let env : String.t List.t = Unix.environment () |> Array.to_list
let global_syscall_offset = ref 0L

let global_brk =
  ref
    ((String.hash (Format.asprintf "%Lx" !global_syscall_offset)
     |> Int64.of_int |> Int64.shift_left)
       24)

let x64_syscall_table (n : Int64.t) : Interop.func_sig Option.t =
  [%log finfo "syscall" "SYSCALL NUM: %Ld" n];
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
  | 1L (* write *) ->
      {
        Interop.params =
          ( [ ("x", T64) ],
            [ Interop.t64; Interop.immutable_charbuffer_of "x"; Interop.id "x" ]
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
  | 8L (* lseek *) ->
      {
        Interop.params = ([], [ Interop.t64; Interop.t64; Interop.t64 ]);
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
                    (0x541BL (* FIONREAD *), Interop.mutable_charbuffer_fixed 4L);
                    (0x40049409L (* BTRFS_IOC_CLONE *), Interop.t64);
                  ],
                  Interop.tany );
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 19L (* readv *) ->
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
                                TPrim (Interop.mutable_charbuffer_of "y");
                                TPrim (TArith (TInt (Id "y")));
                              ] ),
                        Interop.Dependent "x" )));
              Interop.id "x";
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
  | 33L (* dup2 *) ->
      {
        Interop.params = ([], [ Interop.t64; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 41L (* socket *) ->
      {
        Interop.params = ([], [ Interop.t64; Interop.t64; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 42L (* connect *) ->
      {
        Interop.params =
          ( [],
            [ Interop.t64; Interop.mutable_charbuffer_fixed 16L; Interop.t64 ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 60L (* exit *) ->
      { Interop.params = ([], [ Interop.t64 ]); result = Some Interop.t64 }
      |> Option.some
  | 63L (* uname *) ->
      {
        Interop.params = ([], [ Interop.mutable_charbuffer_fixed 325L ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 72L (* fcntl *) ->
      {
        Interop.params = ([], [ Interop.t64; Interop.t64; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 75L (* fdatasync *) ->
      { Interop.params = ([], [ Interop.t64 ]); result = Some Interop.t64 }
      |> Option.some
  | 77L (* ftruncate *) ->
      {
        Interop.params = ([], [ Interop.t64; Interop.t64 ]);
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
  | 80L (* chdir *) ->
      {
        Interop.params = ([], [ Interop.const_string_ptr ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 81L (* fchdir *) ->
      { Interop.params = ([], [ Interop.t64 ]); result = Some Interop.t64 }
      |> Option.some
  | 84L (* rmdir *) ->
      {
        Interop.params = ([], [ Interop.const_string_ptr ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 87L (* unlink *) ->
      {
        Interop.params = ([], [ Interop.const_string_ptr ]);
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
  | 90L (* chmod *) ->
      {
        Interop.params = ([], [ Interop.const_string_ptr; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 91L (* fchmod *) ->
      {
        Interop.params = ([], [ Interop.t64; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 93L (* fchown *) ->
      {
        Interop.params = ([], [ Interop.t64; Interop.t64; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 95L (* umask *) ->
      { Interop.params = ([], [ Interop.t64 ]); result = Some Interop.t64 }
      |> Option.some
  | 96L (* gettimeofday *) ->
      {
        Interop.params =
          ( [],
            [
              Interop.mutable_charbuffer_fixed 16L;
              Interop.mutable_charbuffer_fixed 8L;
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 99L (* sysinfo *) ->
      {
        Interop.params = ([], [ Interop.mutable_charbuffer_fixed 112L ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 102L (* getuid *) ->
      { Interop.params = ([], []); result = Some Interop.t64 } |> Option.some
  | 104L (* getgid *) ->
      { Interop.params = ([], []); result = Some Interop.t64 } |> Option.some
  | 107L (* geteuid *) ->
      { Interop.params = ([], []); result = Some Interop.t64 } |> Option.some
  | 108L (* getegid *) ->
      { Interop.params = ([], []); result = Some Interop.t64 } |> Option.some
  | 115L (* getgroups *) ->
      {
        Interop.params =
          ([ ("x", T64) ], [ Interop.id "x"; Interop.mutable_intbuffer_of "x" ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 137L (* statfs *) ->
      {
        Interop.params =
          ( [],
            [ Interop.const_string_ptr; Interop.mutable_charbuffer_fixed 120L ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 138L (* fstatfs *) ->
      {
        Interop.params =
          ([], [ Interop.t64; Interop.mutable_charbuffer_fixed 120L ]);
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
  | 228L (* clock_gettime *) ->
      {
        Interop.params =
          ([], [ Interop.t64; Interop.mutable_charbuffer_fixed 16L ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 231L (* fgetxattr *) ->
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
  | 258L (* mkdirat *) ->
      {
        Interop.params =
          ([], [ Interop.t64; Interop.const_string_ptr; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 259L (* mknodat *) ->
      {
        Interop.params =
          ( [],
            [ Interop.t64; Interop.const_string_ptr; Interop.t64; Interop.t64 ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 260L (* fchownat *) ->
      {
        Interop.params =
          ( [],
            [
              Interop.t64;
              Interop.const_string_ptr;
              Interop.t64;
              Interop.t64;
              Interop.t64;
            ] );
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
  | 263L (* unlinkat *) ->
      {
        Interop.params =
          ([], [ Interop.t64; Interop.const_string_ptr; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 264L (* renameat *) ->
      {
        Interop.params =
          ( [],
            [
              Interop.t64;
              Interop.const_string_ptr;
              Interop.t64;
              Interop.const_string_ptr;
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 265L (* linkat *) ->
      {
        Interop.params =
          ( [],
            [
              Interop.t64;
              Interop.const_string_ptr;
              Interop.t64;
              Interop.const_string_ptr;
              Interop.t64;
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 266L (* symlinkat *) ->
      {
        Interop.params =
          ( [],
            [ Interop.const_string_ptr; Interop.t64; Interop.const_string_ptr ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 267L (* readlinkat *) ->
      {
        Interop.params =
          ( [ ("x", T64) ],
            [
              Interop.t64;
              Interop.const_string_ptr;
              Interop.mutable_charbuffer_of "x";
              Interop.id "x";
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 268L (* fchmodat *) ->
      {
        Interop.params =
          ([], [ Interop.t64; Interop.const_string_ptr; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 269L (* faccessat *) ->
      {
        Interop.params =
          ([], [ Interop.t64; Interop.const_string_ptr; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 280L (* utimensat *) ->
      {
        Interop.params =
          ( [],
            [
              Interop.t64;
              Interop.const_string_ptr;
              Interop.mutable_charbuffer_fixed 16L;
              Interop.t64;
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 285L (* fallocate *) ->
      {
        Interop.params =
          ([], [ Interop.t64; Interop.t64; Interop.t64; Interop.t64 ]);
        result = Some Interop.t64;
      }
      |> Option.some
  | 316L (* renameat2 *) ->
      {
        Interop.params =
          ( [],
            [
              Interop.t64;
              Interop.const_string_ptr;
              Interop.t64;
              Interop.const_string_ptr;
              Interop.t64;
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 318L (* getrandom *) ->
      {
        Interop.params =
          ( [ ("x", T64) ],
            [ Interop.mutable_charbuffer_of "x"; Interop.id "x"; Interop.t64 ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 332L (* statx *) ->
      {
        Interop.params =
          ( [],
            [
              Interop.t64;
              Interop.const_string_ptr;
              Interop.t64;
              Interop.t64;
              Interop.mutable_charbuffer_fixed 256L;
            ] );
        result = Some Interop.t64;
      }
      |> Option.some
  | 439L (* faccessat2 *) ->
      {
        Interop.params =
          ( [],
            [ Interop.t64; Interop.const_string_ptr; Interop.t64; Interop.t64 ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | 452L (* fchmodat2 *) ->
      {
        Interop.params =
          ( [],
            [ Interop.t64; Interop.const_string_ptr; Interop.t64; Interop.t64 ]
          );
        result = Some Interop.t64;
      }
      |> Option.some
  | _ -> Option.none

let x64_do_syscall (args : Interop.t list) : (Interop.t, String.t) Result.t =
  global_syscall_offset := Int64.add !global_syscall_offset 1L;
  [%log
    finfo "syscall" "SYSCALL ARGS: %a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
         Interop.pp)
      args];
  let* rax, args =
    match args with
    | Interop.VArith (VInt (V64 rax)) :: rest -> Ok (rax, rest)
    | _ -> Error "syscall: invalid syscall number"
  in
  let* res =
    match (rax, args) with
    | 0L, [ VArith (VInt (V64 rdi)); VBuffer rsi; VArith (VInt (V64 rdx)) ] ->
        let retv = Util.read (rdi |> Int64.to_int) rsi rdx in
        [%log
          finfo "syscall" "READ ARG: %Ld %a %Ld" rdi Interop.pp (VBuffer rsi)
            rdx];
        [%log finfo "syscall" "READ RET: %Ld" retv];
        Interop.v64 retv |> Result.ok
    | 1L, [ VArith (VInt (V64 rdi)); VIBuffer rsi; VArith (VInt (V64 rdx)) ] ->
        let retv =
          Util.write (rdi |> Int64.to_int) (Interop.vibuffer_to_string rsi) rdx
        in
        [%log
          finfo "syscall" "WRITE ARG: %Ld %a %Ld" rdi Interop.pp (VIBuffer rsi)
            rdx];
        [%log finfo "syscall" "WRITE RET: %Ld" retv];
        Interop.v64 retv |> Result.ok
    | ( 2L,
        [ VIBuffer sname; VArith (VInt (V64 flags)); VArith (VInt (V64 mode)) ]
      ) ->
        let name = Interop.vibuffer_to_string sname in
        let retv =
          Util.open_ name (flags |> Int64.to_int) (mode |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | 3L, [ VArith (VInt (V64 rdi)) ] ->
        Interop.v64 (Util.close (rdi |> Int64.to_int)) |> Result.ok
    | 4L, [ VIBuffer rsi; VBuffer rdx ] ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv = Util.stat path rdx in
        Interop.v64 retv |> Result.ok
    | 5L, [ VArith (VInt (V64 rdi)); VBuffer rsi ] ->
        let retv = Util.fstat (rdi |> Int64.to_int) rsi in
        Interop.v64 retv |> Result.ok
    | 6L, [ VIBuffer rsi; VBuffer rdx ] ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv = Util.lstat path rdx in
        Interop.v64 retv |> Result.ok
    | ( 8L,
        [
          VArith (VInt (V64 rdi));
          VArith (VInt (V64 rsi));
          VArith (VInt (V64 rdx));
        ] ) ->
        let retv = Util.lseek (rdi |> Int64.to_int) rsi (rdx |> Int64.to_int) in
        Interop.v64 retv |> Result.ok
    | ( 9L,
        [
          VArith (VInt (V64 rdi));
          VArith (VInt (V64 rsi));
          VArith (VInt (V64 rdx));
          VArith (VInt (V64 rcx));
          VArith (VInt (V64 r8));
          VArith (VInt (V64 r9));
        ] ) ->
        if Int64.equal r8 0xffffffffffffffffL then
          if Int64.equal rdi 0L then
            let hval =
              (String.hash (Format.asprintf "%Lx" !global_syscall_offset)
              |> Int64.of_int |> Int64.shift_left)
                24
            in
            Interop.v64 hval |> Result.ok
          else Interop.v64 rdi |> Result.ok
        else Error "Not supported mmap"
    | 11L, [ rdi; rsi ] -> Interop.v64 0L |> Result.ok
    | 12L, [ VArith (VInt (V64 rdi)) ] ->
        if Int64.equal rdi 0L then Interop.v64 !global_brk |> Result.ok
        else (
          global_brk := rdi;
          Interop.v64 rdi |> Result.ok)
    | 16L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 0x5401L)); VBuffer rdx ]
      ->
        (* TCGETS *)
        let retv = Util.tcgets (rdi |> Int64.to_int) rdx in
        Interop.v64 retv |> Result.ok
    | ( 16L,
        [ VArith (VInt (V64 rdi)); VArith (VInt (V64 0x5402L)); VIBuffer rdx ] )
      ->
        (* TCSETS *)
        let* str = Interop.vibuffer_to_string rdx |> Result.ok in
        let retv = Util.tcsets (rdi |> Int64.to_int) str in
        Interop.v64 retv |> Result.ok
    | ( 16L,
        [ VArith (VInt (V64 rdi)); VArith (VInt (V64 0x5403L)); VIBuffer rdx ] )
      ->
        (* TCSETSW *)
        let* str = Interop.vibuffer_to_string rdx |> Result.ok in
        let retv = Util.tcsetsw (rdi |> Int64.to_int) str in
        Interop.v64 retv |> Result.ok
    | 16L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 0x5413L)); VBuffer rdx ]
      ->
        (* TIOCGWINSZ *)
        let retv = Util.tiocgwinsz (rdi |> Int64.to_int) rdx in
        Interop.v64 retv |> Result.ok
    | 16L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 0x541bL)); VBuffer rdx ]
      ->
        (* FIONREAD *)
        let retv = Util.fionread (rdi |> Int64.to_int) rdx in
        Interop.v64 retv |> Result.ok
    | ( 16L,
        [
          VArith (VInt (V64 rdi));
          VArith (VInt (V64 0x40049409L));
          VArith (VInt (V64 rdx));
        ] ) ->
        (* BTRFS_IOC_CLONE *)
        let retv =
          Util.btrfs_ioc_clone (rdi |> Int64.to_int) (rdx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | 16L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 rsi)); VOpaque ] ->
        [%log error "not implemented ioctl for %Ld %Ld" rdi rsi]
    | 19L, [ VArith (VInt (V64 rdi)); VIBuffer rsi; VArith (VInt (V64 rdx)) ] ->
        [%log
          finfo "syscall" "READV ARG: %Ld %a %Ld" rdi Interop.pp (VIBuffer rsi)
            rdx];
        let* readarr =
          Array.map
            (fun x ->
              match x with
              | Interop.VStruct [ VBuffer b; _ ] -> b |> Result.ok
              | Interop.VStruct [ VNullPtr; _ ] -> Bytes.empty |> Result.ok
              | _ ->
                  Format.asprintf "parse error: %a" Interop.pp x |> Result.error)
            rsi
          |> Array.to_list |> Result.join_list
        in
        let readarr_len =
          List.fold_left (fun acc x -> acc + Bytes.length x) 0 readarr
        in
        let newbytes = Bytes.create readarr_len in
        let retv =
          Util.read (rdi |> Int64.to_int) newbytes (readarr_len |> Int64.of_int)
        in
        if Int64.compare retv 0L < 0 then Interop.v64 retv |> Result.ok
        else
          let _ =
            List.fold_left
              (fun acc x ->
                if Int.equal (Bytes.length acc) 0 then acc
                else if Int.compare (Bytes.length acc) (Bytes.length x) < 0 then (
                  Bytes.blit acc 0 x 0 (Bytes.length acc);
                  Bytes.empty)
                else (
                  Bytes.blit acc 0 x 0 (Bytes.length x);
                  Bytes.sub acc (Bytes.length x)
                    (Bytes.length acc - Bytes.length x)))
              newbytes readarr
          in
          Interop.v64 retv |> Result.ok
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
              | Interop.VStruct [ VNullPtr; _ ] -> String.empty |> Result.ok
              | _ ->
                  Format.asprintf "parse error: %a" Interop.pp x |> Result.error)
            rsi
          |> Array.to_list |> Result.join_list
        in
        let writestr = String.concat "" writearr in
        let retv =
          Util.write (Int64.to_int rdi) writestr
            (String.length writestr |> Int64.of_int)
        in
        Interop.v64 retv |> Result.ok
    | 33L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 rsi)) ] ->
        let retv = Util.dup2 (rdi |> Int64.to_int) (rsi |> Int64.to_int) in
        Interop.v64 retv |> Result.ok
    | ( 41L,
        [
          VArith (VInt (V64 rdi));
          VArith (VInt (V64 rsi));
          VArith (VInt (V64 rdx));
        ] ) ->
        let retv =
          Util.socket (rdi |> Int64.to_int) (rsi |> Int64.to_int)
            (rdx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | 42L, [ VArith (VInt (V64 rdi)); VBuffer rsi; VArith (VInt (V64 rdx)) ] ->
        let retv =
          Util.connect (rdi |> Int64.to_int) rsi (rdx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | 60L, [ VArith (VInt (V64 rdi)) ] -> exit (Int64.to_int rdi)
    | 63L, [ VBuffer rdi ] ->
        let retv = Util.uname rdi in
        Interop.v64 retv |> Result.ok
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
                 (Util.dupfd (rdi |> Int64.to_int) (rdx |> Int64.to_int)))
        | 1L (*GETFD*) -> Ok (Interop.v64 (Util.getfd (rdi |> Int64.to_int)))
        | 2L (*SETFD*) ->
            Ok
              (Interop.v64
                 (Util.setfd (rdi |> Int64.to_int) (rdx |> Int64.to_int)))
        | 3L (*GETFL*) -> Ok (Interop.v64 (Util.getfl (rdi |> Int64.to_int)))
        | 4L (*SETFL*) ->
            Ok
              (Interop.v64
                 (Util.setfl (rdi |> Int64.to_int) (rdx |> Int64.to_int)))
        | 8L (*SETOWN*) ->
            Ok
              (Interop.v64
                 (Util.setown (rdi |> Int64.to_int) (rdx |> Int64.to_int)))
        | 9L (*GETOWN*) -> Ok (Interop.v64 (Util.getown (rdi |> Int64.to_int)))
        | 1030L (* F_DUPFD_CLOEXEC *) ->
            Ok
              (Interop.v64
                 (Util.dupfd_cloexec (rdi |> Int64.to_int) (rdx |> Int64.to_int)))
        | _ -> Error (Format.sprintf "unimplemented fcntl %Ld" rsi))
    | 75L, [ VArith (VInt (V64 rdi)) ] ->
        let retv = Util.fdatasync (rdi |> Int64.to_int) in
        Interop.v64 retv |> Result.ok
    | 77L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 rsi)) ] ->
        let retv = Util.ftruncate (rdi |> Int64.to_int) rsi in
        Interop.v64 retv |> Result.ok
    | 79L, [ VBuffer rdi; VArith (VInt (V64 rsi)) ] ->
        let retv = Util.getcwd rdi (Int64.to_int rsi) in
        Interop.v64 retv |> Result.ok
    | 80L, [ VIBuffer rdi ] ->
        let* path = Interop.vibuffer_to_string rdi |> Result.ok in
        let retv = Util.chdir path in
        Interop.v64 retv |> Result.ok
    | 81L, [ VArith (VInt (V64 rdi)) ] ->
        let retv = Util.fchdir (rdi |> Int64.to_int) in
        Interop.v64 retv |> Result.ok
    | 84L, [ VIBuffer rdi ] ->
        let* path = Interop.vibuffer_to_string rdi |> Result.ok in
        let retv = Util.rmdir path in
        Interop.v64 retv |> Result.ok
    | 87L, [ VIBuffer rdi ] ->
        let* path = Interop.vibuffer_to_string rdi |> Result.ok in
        let retv = Util.unlink path in
        Interop.v64 retv |> Result.ok
    | 89L, [ VIBuffer rdi; VBuffer rsi; VArith (VInt (V64 rdx)) ] ->
        let* path = Interop.vibuffer_to_string rdi |> Result.ok in
        let retv = Util.readlink path rsi (rdx |> Int64.to_int) in
        Interop.v64 retv |> Result.ok
    | 90L, [ VIBuffer rdi; VArith (VInt (V64 rsi)) ] ->
        let* path = Interop.vibuffer_to_string rdi |> Result.ok in
        let retv = Util.chmod path (rsi |> Int64.to_int) in
        Interop.v64 retv |> Result.ok
    | 91L, [ VArith (VInt (V64 rdi)); VArith (VInt (V64 rsi)) ] ->
        let retv = Util.fchmod (rdi |> Int64.to_int) (rsi |> Int64.to_int) in
        Interop.v64 retv |> Result.ok
    | ( 93L,
        [
          VArith (VInt (V64 rdi));
          VArith (VInt (V64 rsi));
          VArith (VInt (V64 rdx));
        ] ) ->
        let retv =
          Util.fchown (rdi |> Int64.to_int) (rsi |> Int64.to_int)
            (rdx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | 95L, [ VArith (VInt (V64 rdi)) ] ->
        let retv = Util.umask (rdi |> Int64.to_int) in
        Interop.v64 retv |> Result.ok
    | 96L, [ VBuffer rdi; VBuffer rsi ] ->
        let retv = Util.gettimeofday rdi rsi in
        Interop.v64 retv |> Result.ok
    | 99L, [ VBuffer rdi ] ->
        let retv = Util.sysinfo rdi in
        Interop.v64 retv |> Result.ok
    | 102L, [] -> Interop.v64 (Unix.getuid () |> Int64.of_int) |> Result.ok
    | 104L, [] -> Interop.v64 (Unix.getgid () |> Int64.of_int) |> Result.ok
    | 107L, [] -> Interop.v64 (Unix.geteuid () |> Int64.of_int) |> Result.ok
    | 108L, [] -> Interop.v64 (Unix.getegid () |> Int64.of_int) |> Result.ok
    | 115L, [ VArith (VInt (V64 rdi)); VBuffer rsi ] ->
        let retv = Util.getgroups (rdi |> Int64.to_int) rsi in
        Interop.v64 retv |> Result.ok
    | 137L, [ VIBuffer rdi; VBuffer rsi ] ->
        let* path = Interop.vibuffer_to_string rdi |> Result.ok in
        let retv = Util.statfs path rsi in
        Interop.v64 retv |> Result.ok
    | 138L, [ VArith (VInt (V64 rdi)); VBuffer rsi ] ->
        let retv = Util.fstatfs (rdi |> Int64.to_int) rsi in
        Interop.v64 retv |> Result.ok
    | 161L, [ VIBuffer rsi ] ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv = Util.chroot path in
        Interop.v64 retv |> Result.ok
    | 217L, [ VArith (VInt (V64 rdi)); VBuffer rsi; VArith (VInt (V64 rdx)) ] ->
        let retv =
          Util.getdents64 (rdi |> Int64.to_int) rsi (rdx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | ( 221L (* fadvise64 *),
        [
          VArith (VInt (V64 rdi));
          VArith (VInt (V64 rsi));
          VArith (VInt (V64 rdx));
          VArith (VInt (V64 rcx));
        ] ) ->
        Interop.v64
          (Util.fadvise64 (rdi |> Int64.to_int) rsi rdx (rcx |> Int64.to_int))
        |> Result.ok
    | 228L, [ VArith (VInt (V64 rdi)); VBuffer rsi ] ->
        let retv = Util.clock_gettime rdi rsi in
        Interop.v64 retv |> Result.ok
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
        Interop.v64 retv |> Result.ok
    | 258L, [ VArith (VInt (V64 rdi)); VIBuffer rsi; VArith (VInt (V64 rdx)) ]
      ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.mkdirat (rdi |> Int64.to_int) path (rdx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | ( 259L,
        [
          VArith (VInt (V64 rdi));
          VIBuffer rsi;
          VArith (VInt (V64 rdx));
          VArith (VInt (V64 rcx));
        ] ) ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.mknodat (rdi |> Int64.to_int) path (rdx |> Int64.to_int)
            (rcx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | ( 260L,
        [
          VArith (VInt (V64 rdi));
          VIBuffer rsi;
          VArith (VInt (V64 rdx));
          VArith (VInt (V64 rcx));
          VArith (VInt (V64 r8));
        ] ) ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.fchownat (rdi |> Int64.to_int) path (rdx |> Int64.to_int)
            (rcx |> Int64.to_int) (r8 |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
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
        Interop.v64 retv |> Result.ok
    | 263L, [ VArith (VInt (V64 rdi)); VIBuffer rsi; VArith (VInt (V64 rdx)) ]
      ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.unlinkat (rdi |> Int64.to_int) path (Int64.to_int rdx)
        in
        Interop.v64 retv |> Result.ok
    | ( 264L,
        [
          VArith (VInt (V64 rdi));
          VIBuffer rsi;
          VArith (VInt (V64 rdx));
          VIBuffer rcx;
        ] ) ->
        let* path1 = Interop.vibuffer_to_string rsi |> Result.ok in
        let* path2 = Interop.vibuffer_to_string rcx |> Result.ok in
        let retv =
          Util.renameat (rdi |> Int64.to_int) path1 (rdx |> Int64.to_int) path2
        in
        Interop.v64 retv |> Result.ok
    | ( 265L,
        [
          VArith (VInt (V64 rdi));
          VIBuffer rsi;
          VArith (VInt (V64 rdx));
          VIBuffer rcx;
          VArith (VInt (V64 r8));
        ] ) ->
        let* path1 = Interop.vibuffer_to_string rsi |> Result.ok in
        let* path2 = Interop.vibuffer_to_string rcx |> Result.ok in
        let retv =
          Util.linkat (rdi |> Int64.to_int) path1 (rdx |> Int64.to_int) path2
            (r8 |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | 266L, [ VIBuffer rdi; VArith (VInt (V64 rsi)); VIBuffer rdx ] ->
        let* path1 = Interop.vibuffer_to_string rdi |> Result.ok in
        let* path2 = Interop.vibuffer_to_string rdx |> Result.ok in
        let retv = Util.symlinkat path1 (rsi |> Int64.to_int) path2 in
        Interop.v64 retv |> Result.ok
    | ( 267L,
        [
          VArith (VInt (V64 rdi));
          VIBuffer rsi;
          VBuffer rdx;
          VArith (VInt (V64 rcx));
        ] ) ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.readlinkat (rdi |> Int64.to_int) path rdx (rcx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | 268L, [ VArith (VInt (V64 rdi)); VIBuffer rsi; VArith (VInt (V64 rdx)) ]
      ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.fchmodat (rdi |> Int64.to_int) path (rdx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | 269L, [ VArith (VInt (V64 rdi)); VIBuffer rsi; VArith (VInt (V64 rdx)) ]
      ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.faccessat (rdi |> Int64.to_int) path (rdx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | ( 280L,
        [
          VArith (VInt (V64 rdi));
          VIBuffer rsi;
          VBuffer rdx;
          VArith (VInt (V64 rcx));
        ] ) ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.utimensat (rdi |> Int64.to_int) path rdx (rcx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | ( 280L,
        [
          VArith (VInt (V64 rdi));
          VNullPtr;
          VBuffer rdx;
          VArith (VInt (V64 rcx));
        ] ) ->
        let retv =
          Util.utimensat_pathnull (rdi |> Int64.to_int) rdx (rcx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | ( 285L,
        [
          VArith (VInt (V64 rdi));
          VArith (VInt (V64 rsi));
          VArith (VInt (V64 rdx));
          VArith (VInt (V64 rcx));
        ] ) ->
        let retv =
          Util.fallocate (rdi |> Int64.to_int) (rsi |> Int64.to_int) rdx rcx
        in
        Interop.v64 retv |> Result.ok
    | ( 316L,
        [
          VArith (VInt (V64 rdi));
          VIBuffer rsi;
          VArith (VInt (V64 rdx));
          VIBuffer rcx;
          VArith (VInt (V64 r8));
        ] ) ->
        let* path1 = Interop.vibuffer_to_string rsi |> Result.ok in
        let* path2 = Interop.vibuffer_to_string rcx |> Result.ok in
        let retv =
          Util.renameat2 (rdi |> Int64.to_int) path1 (rdx |> Int64.to_int) path2
            (r8 |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | 318L, [ VBuffer rdi; VArith (VInt (V64 rsi)); VArith (VInt (V64 rdx)) ] ->
        let retv = Util.getrandom rdi rsi (rdx |> Int64.to_int) in
        Interop.v64 retv |> Result.ok
    | ( 332L,
        [
          VArith (VInt (V64 rdi));
          VIBuffer rsi;
          VArith (VInt (V64 rdx));
          VArith (VInt (V64 rcx));
          VBuffer r8;
        ] ) ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.statx (rdi |> Int64.to_int) path (rdx |> Int64.to_int)
            (rcx |> Int64.to_int) r8
        in
        Interop.v64 retv |> Result.ok
    | ( 439L,
        [
          VArith (VInt (V64 rdi));
          VIBuffer rsi;
          VArith (VInt (V64 rdx));
          VArith (VInt (V64 rcx));
        ] ) ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.faccessat2 (rdi |> Int64.to_int) path (rdx |> Int64.to_int)
            (rcx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | ( 452L,
        [
          VArith (VInt (V64 rdi));
          VIBuffer rsi;
          VArith (VInt (V64 rdx));
          VArith (VInt (V64 rcx));
        ] ) ->
        let* path = Interop.vibuffer_to_string rsi |> Result.ok in
        let retv =
          Util.fchmodat2 (rdi |> Int64.to_int) path (rdx |> Int64.to_int)
            (rcx |> Int64.to_int)
        in
        Interop.v64 retv |> Result.ok
    | _ ->
        Error
          (Format.asprintf "unimplemented syscall %Ld ARGS: %a" rax
             (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
                Interop.pp)
             args)
  in
  [%log finfo "syscall" "SYSCALL RET: %a" Interop.pp res];
  res |> Result.ok

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
      ( "libc_start_init",
        ( { Interop.params = ([], []); result = Some Interop.t32 },
          Hide (int @-> returning int) ) );
      ( "libc_exit_fini",
        ( { Interop.params = ([], []); result = Some Interop.t32 },
          Hide (int @-> returning int) ) );
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
  if String.equal fname "libc_start_init" then EventReturn (Interop.v32 0l)
  else if String.equal fname "libc_exit_fini" then EventReturn (Interop.v32 0l)
  else if List.mem fname cgc_funcs then (
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
